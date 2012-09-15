;;; Copyright (c) 2012 Jesper Raftegard <jesper@huggpunkt.org>
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

;;; Let's ignore syntax for a while...

(define (print-to-port* port . stuff)
  (if (every (lambda (o) (or (string? o) (number? o) (char? o)))
             stuff)
      (display (apply conc stuff) port)
      (pretty-print stuff port)))

(define (print-to-port port . stuff)
  (apply print-to-port* (cons port stuff))
  (newline port))

(define (stdout . stuff)
  (apply print-to-port (cons (current-output-port) stuff)))

(define (stdout* . stuff)
  (apply print-to-port* (cons (current-output-port) stuff)))

(define (stderr . stuff)
  (apply print-to-port (cons (current-error-port) stuff)))

(define (stderr* . stuff)
  (apply print-to-port* (cons (current-error-port) stuff)))

(define (debug . stuff)
  (if verbosity
      (apply stderr stuff)))

(define (debug* . stuff)
  (if verbosity
      (apply stderr* stuff)))

(define (debug-wrap-with-prefix prefix stuff)
  (begin
    (if verbosity
	(if (string? stuff)
            (stderr (conc prefix stuff))
            (stderr (cons prefix (list stuff)))))
    stuff))

(define (debug-wrap-with-prefix* prefix stuff)
  (begin
    (if verbosity
	(if (string? stuff)
            (stderr* (conc prefix stuff))
            (stderr* (cons prefix (list stuff)))))
    stuff))

(define (debug-wrap stuff)
  (begin
    (debug-wrap-with-prefix "" stuff)
    stuff))

(define (debug-wrap* stuff)
  (begin
    (debug-wrap-with-prefix* "" stuff)
    stuff))

(define (debug-wrap-with-parser parser stuff)
  (begin
    (debug-wrap (apply parser (list stuff)))
    stuff))

(define (debug-wrap-with-parser* parser stuff)
  (begin
    (debug-wrap* (apply parser (list stuff)))
    stuff))
