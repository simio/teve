#| Copyright (c) 2012, 2013 Jesper Raftegard <jesper@huggpunkt.org>
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

(require-extension json)

;; Recurse through the vector/alist mess returned by json-read,
;; converting vectors to alists.
(define (sanitise-json-input obj)
  (cond ((null? obj) obj)
        ((pair? obj) (cons (sanitise-json-input (car obj))
                           (sanitise-json-input (cdr obj))))
        ((vector? obj) (sanitise-json-input (vector->list obj)))
        (else obj)))

;;; Read with json-read and sanitise with sanitise-json-input
(define (json-read-and-sanitise)
  (sanitise-json-input (handle-exceptions exn #f (json-read))))

(define json-ref quick-ref)

