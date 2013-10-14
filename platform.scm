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

(use srfi-1 data-structures files)

(require-extension miscmacros)

(define (make-platform)
  (let* ((program-filename (let ((actual (pathname-file (program-name))))
                             (if (equal? "csi" actual) "teve" actual)))
         (home-dir (get-environment-variable "HOME"))
	 (user-data-dir (conc home-dir "/." program-filename))
         (system-config-file (if* (file-exists?
                                   (conc "/etc/" program-filename ".conf"))
                                  it
                                  #f))
         (user-config-file (find
                            (lambda (x) (and (string? x) (file-exists? x)))
                            (list (get-environment-variable "TEVE_RC")
                                  (conc user-data-dir "/config")
                                  (conc home-dir "/." program-filename "rc"))))
         (cache-dir (conc user-data-dir "/cache"))
         (values `((program-filename . ,program-filename)
                   (home-dir . ,home-dir)
                   (etc-dir . "/etc")
                   (user-data-dir . ,user-data-dir)
                   (user-config-file . ,user-config-file)
                   (system-config-file . ,system-config-file)
                   (cache-dir . ,cache-dir))))
    (lambda key
      (if (and (list? key)
               (> (length key) 0))
          (alist-ref (car key) values)
          values))))

(define *platform* (make-platform))

(define (create-user-config-dirs)
  (create-directory (*platform* 'user-data-dir) #t)
  (create-directory (*platform* 'cache-dir) #t))

(define (shell-escape str)
  (if (eq? 'windows (software-type))
      (conc #\" str #\")
      (let ((special-chars (string->list " &|$\"'\\()[]{}<>#~=;,*")))
        (let escape ((rest (string->list str))
                     (result '()))
          (cond
           ((null? rest)
            (list->string (reverse result)))
           ((member (car rest) special-chars)
            (escape (cdr rest) (cons (car rest) (cons #\\ result))))
           (else
            (escape (cdr rest) (cons (car rest) result))))))))
