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
(require-extension miscmacros http-client json)

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

;; Recurse through the vector/alist mess returned by json-read,
;; converting vectors to alists.
(define (sanitise-json-input obj)
  (cond ((null? obj) obj)
        ((pair? obj) (cons (sanitise-json-input (car obj))
                           (sanitise-json-input (cdr obj))))
        ((vector? obj) (sanitise-json-input (vector->list obj)))
        (else obj)))

;;; Download and sanitise a json object from url
(define (download-json url)
  (handle-exceptions
      exn #f
    (sanitise-json-input (with-input-from-request url #f json-read))))

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
  (let ((index (and (string? url)
                    (string-contains-ci url "://"))))
    (if index
        (string-take url index)
        #f)))

;;; Add a query var=value pair to a url
;;; Return values:
;;;   A string
;;; Unspecified if url, var or val is not a string or char.
(define (add-http-get-query-var url var val)
  (if (string? url)
      (conc url
            (if (string-index url #\?)
                "&"
                "?")
            var #\= val)
      #f))

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
  
(define (make-rnd-string len . tail)
  (let* ((str "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")
         (char (string-ref str (random (string-length str))))
         (appendix (if (null? tail) "" (car tail))))
    (if (> 1 len)
        appendix
        (make-rnd-string (- len 1) (conc char appendix)))))
  
(define (shell-escape str)
  (let escape ((rest (string->list str))
               (result '()))
    (cond
     ((null? rest)
      (list->string (reverse result)))
     ((or (eq? #\& (car rest))
          (eq? #\| (car rest))
          (eq? #\\ (car rest)))
      (escape (cdr rest) (cons (car rest) (cons #\\ result))))
     (else
      (escape (cdr rest) (cons (car rest) result))))))

;;; s/needle/replacement/g
(define (string-replace-every needle replacement string)
  (let loop ((rest string)
             (chunks '()))
    (if* (string-contains rest needle)
         (loop (string-drop rest (+ it (string-length needle)))
               (cons replacement (cons (string-take rest it) chunks)))
         (apply conc (reverse (cons rest chunks))))))
        
;;; Get (un)quoted value of first html attribute style key-value pair.
(define (first-html-attribute attribute source)
  (and-let* ((attr-index (string-contains-ci source attribute))
             (begin-index (+ 1 (string-index source #\" attr-index)))
             (end-index (string-index source #\" begin-index) begin-index))
    (substring/shared source begin-index end-index)))
             
