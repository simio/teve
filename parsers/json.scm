;;; Copyright (c) 2012, 2013 Jesper Raftegard <jesper@huggpunkt.org>
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

(module json-parser (sanitise-json-input
                     unsanitise-json-input
                     json-read->alist-tree
                     json-ref
                     json-printer)
(import scheme chicken srfi-1 srfi-69 ports
        json miscmacros
        prelude misc-helpers)

;; Recurse through the vector/alist mess returned by json-read,
;; converting vectors to alists.
(define (sanitise-json-input obj)
  (cond ((null? obj) obj)
        ((pair? obj) (cons (sanitise-json-input (car obj))
                           (sanitise-json-input (cdr obj))))
        ((vector? obj) (sanitise-json-input (vector->list obj)))
        (else obj)))

;; Reverse the above
(define (unsanitise-json-input obj #!optional (first-car #t))
  (let ((unsanitise-pair (lambda (x)
                           (cons (if (symbol? (car x))
                                     (symbol->string (car x))
                                     (car x))
                                 (unsanitise-json-input (cdr x))))))
    (cond ((null? obj) obj)
          ((symbol? obj) (symbol->string obj))
          ;; Convert alists to hash-tables
          ((and (list? obj)
                (not (null? obj))
                (every (lambda (x) (and (pair? x) (not (list? (cdr x)))))
                       obj))
           (let ((lis (cons (unsanitise-json-input (car obj) #t)
                            (unsanitise-json-input (cdr obj) #f))))
             (if first-car
                 (alist->hash-table lis)
                 lis)))
          ;; Walk down any lists that are not alists
          ((pair? obj)
           (cons (unsanitise-json-input (car obj))
                 (unsanitise-json-input (cdr obj))))
          (else obj))))
          
          

;;; Read with json-read and sanitise with sanitise-json-input
(define (json-read->alist-tree)
  (sanitise-json-input (ignore-errors (json-read))))

(define json-ref quick-ref)

(define (json-printer data)
  (with-output-to-string
    (lambda ()
      (json-write (unsanitise-json-input data)))))

)
