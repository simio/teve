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

(use data-structures files posix)

(require-extension miscmacros)

(define (make-platform)
  (let ((values `((program-filename . ,(pathname-file (program-name)))
                  (home-dir . ,(cadddr
                                (cddr (user-information (current-user-id)))))
                  (etc-dir . "/etc")
                  (userconf-file-extension . "rc")
                  (systemconf-file-extension . ".conf"))))
    (lambda (key)
      (alist-ref key values))))

(define *platform* (make-platform))

(define (system-config-filename)
  (let ((filename (conc (*platform* 'etc-dir)
                        "/" program-display-name
                        (*platform* 'systemconf-file-extension))))
    (and (file-exists? filename) filename)))

(define (user-config-filename)
  (let ((filename (or (get-environment-variable "TEVE_RC")
                      (conc (*platform* 'home-dir)
                            "/." program-display-name
                            (*platform* 'userconf-file-extension)))))
    (and (file-exists? filename) filename)))
