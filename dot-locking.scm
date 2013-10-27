;;; These functions are intended to be feature equivalent with the
;;; dot-locking egg, from which much of the code is borrowed. The
;;; dot-locking egg is written by Olin Shivers and BSD licensed.
;;;
;;; http://wiki.call-cc.org/eggref/4/dot-locking
;;;
;;; The dot-locking egg tries to create a hard link between a file in
;;; /tmp (created with create-temporary-directory) and the directory
;;; of the file being locked. Hard links cannot span partitions,
;;; though, so this will fail whenever someone chooses to mount /tmp
;;; as a partition of its own (and someone should).
;;;
;;; This alternative implementation instead creates temporary
;;; files in the same directory as the file being locked, thereby
;;; circumventing the above problem.

;;; Copyright (c) Olin Shivers
;;; All rights reserved.
;;; 
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;; 
;;; 1. Redistributions of source code must retain the above copyright
;;;    notice, this list of conditions and the following disclaimer.
;;; 2. Redistributions in binary form must reproduce the above copyright
;;;    notice, this list of conditions and the following disclaimer in the
;;;    documentation and/or other materials provided with the distribution.
;;; 3. The name of the authors may not be used to endorse or promote products
;;;    derived from this software without specific prior written permission.
;;; 
;;; THIS SOFTWARE IS PROVIDED BY THE AUTHORS ``AS IS'' AND ANY EXPRESS OR
;;; IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
;;; OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
;;; IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY DIRECT, INDIRECT,
;;; INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
;;; NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
;;; THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;;; Copyright (c) 2013 Jesper Raftegard <jesper@huggpunkt.org>
;;;
;;; Permission to use, copy, modify, and distribute this software for any
;;; purpose with or without fee is hereby granted, provided that the above
;;; copyright notice and this permission notice appear in all copies.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
;;; WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
;;; MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
;;; ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
;;; WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
;;; ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
;;; OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

(module dot-locking (release-dot-lock
                     break-dot-lock
                     obtain-dot-lock
                     with-dot-lock
                     with-dot-lock*)
(import-for-syntax posix files srfi-18)
(import scheme chicken posix extras data-structures srfi-18 files)

(define (file->lock file)
  (let ((path (conc file ".lock")))
    (normalize-pathname
     (if (absolute-pathname? path)
         path
         (make-absolute-pathname (current-directory) path)))))

(define (create-rnd-lock file)
  (let ((filename (conc (file->lock file)
                        (number->string (random 252016003)))))
    (with-output-to-file filename
      (lambda () (print "lock")))
    filename))

(define (release-dot-lock file)
  (delete-file* (file->lock file)))

(define break-dot-lock release-dot-lock)

(define (attempt-lock file)
  (let ((rnd-lock (create-rnd-lock file))
        (lock (file->lock file)))
    (handle-exceptions ex
        (begin
          (delete-file rnd-lock)
          #f)
      (file-link rnd-lock lock)
      (delete-file rnd-lock)
      #t)))

(define (obtain-dot-lock file . args)
  (let-optionals args ((retry-seconds 1)
                       (retry-number #f)
                       (stale-time 300))
    (let ((lock-file-name (file->lock file))
          (retry-interval retry-seconds))
      (let loop ((retry-number retry-number)
                 (broken? #f))
        (cond
         ((attempt-lock file)
          (if broken? 'broken #t))
         ((and stale-time
               (handle-exceptions ex #f
                 (> (current-seconds)
                    (+ (file-modification-time lock-file-name)
                       stale-time))))
          (break-dot-lock file)
          (loop retry-number #t))
         (else
          (thread-sleep!
           (+ 1 (quotient (* retry-interval 3) 4)
              (random (quotient retry-interval 2))))
          (cond ((not retry-number)
                 (loop retry-number broken?))
                ((> retry-number 0)
                 (loop (- retry-number 1) broken?))
                (else
                 #f))))))))
          
(define (with-dot-lock* file thunk)
  (dynamic-wind
    (lambda ()
      (obtain-dot-lock file))
    (lambda ()
      (call-with-values thunk
        (lambda a
          (release-dot-lock file)
          (apply values a))))
    (lambda ()
      (release-dot-lock file))))

(define-syntax with-dot-lock
  (syntax-rules ()
    ((with-dot-lock file body ...)
     (with-dot-lock* file (lambda () body ...)))))

)
