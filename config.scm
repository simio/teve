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

(require-extension miscmacros)

(include "misc-helpers.scm")

;;; Default pre-configuration values
(define (make-configuration)
  (let ((values '((operators
                   (default . play)
                   (play? . #f)
                   (download? . #f)
                   (list? . #f)
                   (stream-id . #f)
                   (video-id . #f)
                   (output-filename . #f))
                  (preferences
                   (ideal-bitrate . 2500)
                   (ideal-pixel-width . 1280)))))
    (lambda args
      (cond
       ((null? args) values)
       ((equal? set: (car args))
        (set! values (apply atree-update (cons values (cdr args))))
        values)
       (else
        (apply atree-ref values args))))))

(define *cfg* (make-configuration))

