#| Copyright (c) 2012 Jesper Raftegard <jesper@huggpunkt.org>
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

(require-extension srfi-1 srfi-13)
(require-extension http-client json)

;;; Accessor for values in a tree returned by parse-json.
;;; Keys are strings (for use with alists) or numbers (for use with list-ref).
;;; Use multiple keys to go deeper into the tree. For example,
;;; (json-ref tree "vids" 5 "url") is "url" in item 5 in "vids" in tree.
;;;
;;; Return values:
;;;   A list or a pair  (if the specified key exists and its value isn't #f)
;;;   #f                (otherwise)
(define (json-ref obj . keys)
  (cond ((null? keys) obj)
        ((and (number? (car keys))
              (< (car keys) (length obj)))
         (apply json-ref (cons (list-ref obj (car keys)) (cdr keys))))
        ((string? (car keys))
         (apply json-ref (cons (cdr (assoc (car keys) (filter pair? obj)))
                               (cdr keys))))
        (else #f)))

;;; Download and sanitise a json object from url
(define (download-json url)
  ;; Recurse through the vector/alist mess returned by json-read,
  ;; converting vectors to alists.
  (define (sanitise obj)
    (cond ((null? obj) obj)
          ((pair? obj) (cons (sanitise (car obj)) (sanitise (cdr obj))))
          ((vector? obj) (sanitise (vector->list obj)))
          (else obj)))
  (handle-exceptions
   exn #f
   (sanitise (with-input-from-request url #f json-read))))

(define-syntax not-if
  (syntax-rules ()
    ((not-if test value)
     (if test (not value) value))))

;;; Get transport protocol identifier from a URL.
;;; Return values:
;;;   A string          (if url contains "://")
;;;   #f                (otherwise)
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
;;;   An alist          (if input is valid)
;;;   #f                (otherwise)
(define (varlist->alist str)
  (handle-exceptions
   exn #f
   (let make-pairs ((raw-pairs (string-split str ","))
                    (result '()))
     (if (null? raw-pairs)
         result
         (make-pairs
          (cdr raw-pairs)
          (if (string-contains (car raw-pairs) "=")
              (cons (let ((pair (string-split (car raw-pairs) "=")))
                      (cons (car pair) (if (string->number (cadr pair))
                                           (string->number (cadr pair))
                                           (cadr pair))))
                    result)
              result))))))

;;; Make a pair out of strings like "1x2" or "1024X768"
(define (x-sep-resolution->pair str)
  (let find-split ((chars (string->list str))
                   (x-res '()))
    (cond ((null? (cdr chars)) #f)
          ((or (eqv? #\x (car chars))
               (eqv? #\X (car chars)))
           (cons (string->number (apply conc (reverse x-res)))
                 (string->number (apply conc (cdr chars)))))
          (else
           (find-split (cdr chars) (cons (car chars) x-res))))))

;;; Get cdr of PAIR, or eval to #f if it's not a pair (CDr If Pair)
(define-syntax cdip
  (syntax-rules ()
    ((cdip expr)
     (let ((val expr))
       (if (pair? val)
           (cdr val)
           #f)))))

;;; Get car of PAIR, or eval to #f if it's not a pair (CAr If Pair)
(define-syntax caip
  (syntax-rules ()
    ((caip expr)
     (let ((val expr))
       (if (pair? val)
           (car val)
           #f)))))
