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

(use srfi-1 data-structures files posix)

(require-extension miscmacros)

(define (platform-dir-slashes str)
  (let ((needle (if (eq? (software-type) 'windows) #\/ #\\))
        (replacement (if (eq? (software-type) 'windows) #\\ #\/)))
    (and (string? str)
         (string-map (lambda (c) (if (char=? c needle) replacement c))
                     str))))

(define (program-available? str)
  (let ((available? (if (eq? 'windows (software-type))
			(or (file-exists? str) (file-exists? (conc str ".exe")))
			(let*-values (((in out pid) (process (conc "which " str " 2> /dev/null")))
				      ((pid ok status) (process-wait pid)))
			  (= 0 status)))))
    (debug* prepend: (conc "Checking availability of " str)
            available?)))

(define (make-platform)
  (let* ((nix/win (lambda (nix win)
		    (if (eq? (software-type) 'windows)
			win
			nix)))
	 (program-filename (let ((actual (pathname-file (program-name))))
                             (if (equal? "csi" actual) "teve" actual)))
         (home-dir (nix/win (get-environment-variable "HOME")
			    (get-environment-variable "HOMEPATH")))
	 (user-conf-dir (nix/win (get-environment-variable "HOME")
				 (get-environment-variable "APPDATA")))
	 (user-data-dir (platform-dir-slashes (conc user-conf-dir "/" (nix/win
								       (conc "." program-filename)
								       (conc program-filename)))))
	 (etc-dir (if (eq? (software-type) 'windows)
		      (get-environment-variable "ALLUSERSPROFILE")
		      "/etc"))
         (system-config-file (if* (file-exists? (nix/win (conc etc-dir program-filename ".conf")
							 (conc etc-dir program-filename ".ini")))
                                  it
                                  #f))
         (user-config-file (platform-dir-slashes
			    (find
			     (lambda (x) (and (string? x) (file-exists? x)))
			     (list (get-environment-variable "TEVE_RC")
				   (nix/win
				    (conc user-data-dir "/config")
				    (conc user-data-dir "/settings.ini"))
				   (nix/win (conc user-conf-dir "/." program-filename "rc")
					    (conc user-conf-dir "/" program-filename ".ini"))))))
         (cache-dir (platform-dir-slashes (conc user-data-dir "/cache")))
         (values `((program-filename . ,program-filename)
                   (home-dir . ,home-dir)
                   (etc-dir . ,etc-dir)
                   (user-data-dir . ,user-data-dir)
		   (user-config-dir . ,user-conf-dir)
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
