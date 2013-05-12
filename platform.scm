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

(use srfi-1 data-structures files posix)

(require-extension miscmacros)

(define (make-platform)
  (let* ((program-filename (pathname-file (program-name)))
         (home-dir (cadddr (cddr (user-information (current-user-id)))))
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
         (cache-dir (if (file-exists? (conc user-data-dir "/cache")) it #f))
         (values `((program-filename . ,program-filename)
                   (home-dir . ,home-dir)
                   (etc-dir . "/etc")
                   (user-data-dir . ,user-data-dir)
                   (user-config-file . ,user-config-file)
                   (system-config-file . ,system-config-file)
                   (cache-dir . ,cache-dir))))
    (lambda (key)
      (alist-ref key values))))

(define *platform* (make-platform))
