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

(module misc-helpers *
(import scheme chicken srfi-1 srfi-13 data-structures extras
        miscmacros http-client intarweb uri-common ssax
        stdouterr prelude)

;;; Get transport protocol identifier from a URL.
;;; Return values:
;;;   A string          (if uri contains "://")
;;;   #f                (otherwise)
;;; Unspecified if uri is not a string or char.
(define (uri->protocol uri)
  (let ((index (and (string? uri)
                    (string-contains-ci uri "://"))))
    (if index
        (string-take uri index)
        #f)))

;;; Add a query var=value pair to a uri
;;; Return values:
;;;   A string
;;; Unspecified if uri, var or val is not a string or char.
(define (add-http-get-query-var uri var val)
  (if (string? uri)
      (conc uri
            (if (string-index uri #\?)
                "&"
                "?")
            var #\= val)
      #f))

;;; Converts a string of the form "VAR=VAL,VAR2=VAL2,..." to an alist.
;;; Numerical values are converted to numbers, while everything else
;;; is strings.
;;; Return values:
;;;   An alist          (if input is valid)
;;;   #f                (otherwise)
(define (varlist->alist str . tail)
  (let ((splitter (if (null? tail) "," (car tail)))
        (setter (if (or (null? tail) (null? (cdr tail))) "=" (cadr tail))))
    (handle-exceptions
        exn #f
      (let make-pairs ((raw-pairs (string-split str splitter))
                       (result '()))
        (if (null? raw-pairs)
            result
            (make-pairs
             (cdr raw-pairs)
             (if (string-contains (car raw-pairs) setter)
                 (cons (let ((pair (string-split (car raw-pairs) setter)))
                         (cons (car pair)
                               (cond
                                ((string->number (cadr pair))
                                 (string->number (cadr pair)))
                                ((string=? "#f" (cadr pair))
                                 #f)
                                ((string=? "#t" (cadr pair))
                                 #t)
                                ((and (< 0 (string-length (cadr pair)))
                                      (eq? #\' (string-ref (cadr pair) 0)))
                                 (string->symbol (string-drop (cadr pair) 1)))
                                (else
                                 (cadr pair)))))
                       result)
                 result)))))))

;;; Finds the first occurence of a substring 'to-this in a string and
;;; drops all chars up to and including this char, returning the
;;; remainder of the string. If the string does not contain the
;;; substring, the whole string is returned.
(define (string-drop-to str to-this)
  (let* ((splitter (->string to-this))
         (pos (string-contains str splitter)))
    (if pos
        (string-drop str (+ pos (string-length splitter)))
        str)))

;;; Make a pair out of strings like "1x2" or "1024X768"
;;; If impossible, return #f
(define (x-sep-resolution->pair str)
  (let find-split ((chars (string->list str))
                   (x-res '()))
    (cond ((null? chars) #f)
          ((or (eqv? #\x (car chars))
               (eqv? #\X (car chars)))
           (let ((left (string->number (apply conc (reverse x-res))))
                 (right (if (< 1 (length chars))
                            (string->number (apply conc (cdr chars)))
                            0)))
             (if (and left right)
                 (cons left right)
                 #f)))
          (else
           (find-split (cdr chars) (cons (car chars) x-res))))))

(define (make-rnd-string len . tail)
  (let* ((str "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")
         (char (string-ref str (random (string-length str))))
         (appendix (if (null? tail) "" (car tail))))
    (if (> 1 len)
        appendix
        (make-rnd-string (- len 1) (conc char appendix)))))

;;; Get (un)quoted value of first html attribute style key-value pair.
(define (first-html-attribute attribute source . tail)
  (let ((quote-char (if (null? tail) #\" (car tail))))
    (and-let* ((attr-index (string-contains-ci source attribute))
               (begin-index (+ 1 (string-index source quote-char attr-index)))
               (end-index (string-index source quote-char begin-index)))
      (substring/shared source begin-index end-index))))

;;; Thunk which reads XML from current-input-port and returns sxml.
;;; The ssax does not have a thunk reader (like json-read of the json egg).
(define (xml-read)
  (ssax:xml->sxml (current-input-port) '()))

;;; Fed with a string containing a uri, it will return
;;; "scheme://hostname[:non-default-port]"
(define (uri->base-path str)
  (and-let* ((uri (uri-reference str)))
    (conc (uri-scheme uri) "://" (uri-host uri)
          (if* (uri-port uri)
               (if (nor (equal? it 80)
                        (string=? (uri-scheme uri) "http"))
                   (conc ":" it)
                   "")))))

;;; Try to produce a uri in a string, no matter what is supplied.
;;; Failure evaluates to #f.
(define (->string/uri obj)
  (cond
   ((request? obj) (uri->string (request-uri obj)))
   ((uri? obj) (uri->string obj))
   ((string? obj) obj)
   (else #f)))

)
