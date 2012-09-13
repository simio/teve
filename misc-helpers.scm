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

(require-extension srfi-13)

;;; Recurse through the vector/alist mess returned by json-read,
;;; converting vectors to alists.
(define (json-vector->alist/deep vector)
  (cond ((null? vector) vector)
        ((pair? vector) (cons (json-vector->alist/deep (car vector))
                              (json-vector->alist/deep (cdr vector))))
        ((vector? vector) (json-vector->alist/deep (vector->list vector)))
        (else vector)))

;;; Accessor for values in a tree returned by json->vector->alist/deep
;;; keys are strings (for use with alists) or numbers (for use with list-ref).
;;; Use multiple keys to go deeper into the tree. For example,
;;; (json-ref tree "vids" 5 "url") corresponds to tree["vids"][5]["url"] in pseudocode.
;;;
;;; Return values:
;;;   A list or a pair	(if the specified key exists and its value isn't #f)
;;;   #f		(otherwise)
(define (json-ref json . keys)
  (cond ((null? keys) json)
        ((and (number? (car keys))
              (< (car keys) (length json)))
         (apply json-ref (cons (list-ref json (car keys)) (cdr keys))))
        ((string? (car keys))
         (let ((pairs-only (filter-map (lambda (x) (if (pair? x) x #f)) json)))
           (apply json-ref (cons (cdr (assoc (car keys) pairs-only)) (cdr keys)))))
        (else #f)))

;;; Get transport protocol identifier from a URL.
;;; Return values:
;;;   A string		(if url contains "://")
;;;   #f		(otherwise)
;;; Unspecified if url is not a string or char.
(define (url->protocol url)
  (let ((index (string-contains-ci url "://")))
    (if index
        (string-take url index)
        #f)))

;;; Add a query var=value pair to a url
;;; Return values:
;;;   A string
;;; Unspecified if url, var or val is not a string or char.
(define (add-http-get-query-var url var val)
  (conc url
        (if (string-index url #\?)
            "&"
            "?")
        var #\= val))

;;; Converts a string of the form "VAR=VAL,VAR2=VAL2,..." to an alist.
;;; Numerical values are converted to numbers, while everything else is strings.
;;; Return values:
;;;   An alist		(if input is valid)
;;;   #f		(otherwise)
(define (varlist->alist str)
  (handle-exceptions
   exn #f
   (let make-pairs ((raw-pairs (string-split str ","))
                    (result '()))
     (if (null? raw-pairs)
         result
         (make-pairs (cdr raw-pairs)
                     (if (string-contains (car raw-pairs) "=")
                         (cons (let ((pair (string-split (car raw-pairs) "=")))
                                 (cons (car pair) (if (string->number (cadr pair))
                                                      (string->number (cadr pair))
                                                      (cadr pair))))
                               result)
                         result))))))
    
