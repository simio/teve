;;; These functions are intendet to be feature equivalent with the dot-locking
;;; egg, from which much of the code is borrowd. The dot-locking egg is written
;;; by Olin Shivers and BSD licensed.
;;;
;;; http://wiki.call-cc.org/eggref/4/dot-locking
;;;
;;; The dot-locking egg tries to create a hard link between a file in /tmp
;;; (or wherever create-temporary-directory puts its files) and the directory
;;; of the file being locked, it is only capable of locking files on the same
;;; hard disk partition as /tmp. On my system, /tmp is on a partition of its
;;; own, rendering this egg useless.
;;;
;;; This alternative implementation instead creates any temporary files in the
;;; directory of the file being locked.

#| Copyright (c) 2013 Jesper Raftegard <jesper@huggpunkt.org>
 | 
 | Permission to use, copy, modify, and distribute this software for any
 | purpose with or without fee is hereby granted, provided that the above
 | copyright notice and this permission notice appear in all copies.
 | 
 | THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 | WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 | MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 | ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 | WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 | ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 | OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 |#

(use posix extras srfi-18 files)

(include "talk-is-cheap.scm")

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
      (lambda () (stdout "lock")))
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
