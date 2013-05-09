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
(require-extension miscmacros http-client json ssax)

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

;;; Accessor for values in a tree, as those returned by parse-json.
;;; Keys are strings (for use with alists) or numbers (for use with list-ref).
;;; Use multiple keys to go deeper into the tree. For example,
;;; (quick-ref tree "vids" 5 "url") is "url" in item 5 in "vids" in tree.
;;;
;;; Return values:
;;;   A list or cdr of a pair   (if the key exists and its value isn't #f)
;;;   #f                        (otherwise)
;;;
;;; Should produce an error if a negative list index is supplied.
(define (quick-ref obj . keys)
  (cond ((null? keys) obj)
        ((not (list? obj)) #f)
        ((and (number? (car keys))
              (list? obj)
              (< (car keys) (length obj)))
         (apply quick-ref (cons (list-ref obj (car keys)) (cdr keys))))
        ((or (symbol? (car keys))
             (string? (car keys)))
         (apply quick-ref (cons (cdip (assoc (car keys) (filter pair? obj)))
                                (cdr keys))))
        (else #f)))

(define json-ref quick-ref)

;;; UNTESTED Accessor for values in an alist tree.
;;;
;;; Return values:
;;;   Any non-false value	(for any non-false existant value)
;;;   #f			(otherwise)
(define (atree-ref tree . branches)
  (cond ((null? branches) tree)		; Requested branch/leaf.
        ((not (pair? tree)) #f)		; Leaf found, but expected branch.
        (else
         (apply atree-ref (cons (alist-ref (car branches) tree equal? #f)
                                (cdr branches))))))

;;; UNTESTED Updater for values in an alist tree.
;;;
;;; Return values:
;;;   An updated tree		(if the update was sucessful)
;;;   #f			(if the update failed)
(define (atree-update tree value . branches)
  (cond ((or (null? branches)
             (atom? tree)
             (not (every atom? branches)))
         #f)
        ((null? (cdr branches))
         (alist-update (car branches) value tree equal?))
        ((and (not (null? (cdr branches)))	 ; Leaf of new branch requested
              (not (assoc (car branches) tree)))
         (apply atree-update (cons
                              (cons (cons (car branches) '()) tree)
                              (cons value branches))))
        (else
         (alist-update (car branches)
                       (apply atree-update
                         (cons (atree-ref tree (car branches))
                               (cons value (cdr branches))))
                       tree
                       equal?))))

;;; UNTESTED Deep merge of atrees. Overlays one atree on top of another.
;;; The overlay tree will replace any values present in the base.
(define (atree-merge base overlay)
  (cond ((null? overlay) base)
        ((not (pair? (car overlay))) #f)
        ((or (atom? (cdar overlay))
             (atom? (atree-ref base (caar overlay))))	; #f if not found
         (atree-merge
          (atree-update base (cdar overlay) (caar overlay))
          (cdr overlay)))
        (else
         (atree-merge
          (atree-update base
                        (atree-merge (atree-ref base (caar overlay))
                                     (cdar overlay))
                        (caar overlay))
          (cdr overlay)))))

;;; UNTESTED Fold many atrees into one.
;;; Values present in the first supplied tree will replace those present
;;; in the second supplied tree, and so forth from left to right.
(define atree-fold
  (lambda trees
    (fold atree-merge '() trees)))

;; Recurse through the vector/alist mess returned by json-read,
;; converting vectors to alists.
(define (sanitise-json-input obj)
  (cond ((null? obj) obj)
        ((pair? obj) (cons (sanitise-json-input (car obj))
                           (sanitise-json-input (cdr obj))))
        ((vector? obj) (sanitise-json-input (vector->list obj)))
        (else obj)))

;;; Return a delayed download. If a second parameter is supplied,
;;; it is used as a reader. The default is read-string.
(define (delay-download url . tail)
  (let ((reader (if (null? tail) read-string (car tail))))
    (delay
      (handle-exceptions exn #f
        (with-input-from-request url #f reader)))))

;;; Download an XML document object from url
(define (download-xml url)
  (delay-download url xml-read))

;;; Read with json-read and sanitise with sanitise-json-input
(define (json-read-and-sanitise)
  (sanitise-json-input (handle-exceptions exn #f (json-read))))

;;; Download and sanitise a json object from url
(define (download-json url)
  (delay-download url json-read-and-sanitise))

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
                         (cons (car pair) (cond
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

(define (shell-escape str)
  (let ((special-chars (string->list "&|$\"'\\()[]{}<>#~=;,*")))
    (let escape ((rest (string->list str))
                 (result '()))
      (cond
       ((null? rest)
        (list->string (reverse result)))
       ((member (car rest) special-chars)
        (escape (cdr rest) (cons (car rest) (cons #\\ result))))
       (else
        (escape (cdr rest) (cons (car rest) result)))))))

;;; s/needle/replacement/g
(define (string-replace-every needle replacement string)
  (let loop ((rest string)
             (chunks '()))
    (let ((index (string-contains rest needle)))
      (cond
       ((= 0 (string-length needle))
        string)
       (index
        (loop (string-drop rest (+ index (string-length needle)))
              (cons replacement (cons (string-take rest index) chunks))))
       (else
        (apply conc (reverse (cons rest chunks))))))))
        
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

;;; While the json egg creates trees we sanitise into alists, the ssax
;;; egg creates trees where even key/value pairs are stored as proper
;;; lists (with a length of 2). If this sxml-ref procedure is used to
;;; refer to the cdr of such a pair, it returns the cadr instead of
;;; the cdr, which means it pretends it is accessing an alist. This
;;; way, the usage of it comes closer to that of json-ref.
(define (sxml-ref data . keys)
  (let ((val (apply quick-ref (cons data keys))))
    (if (and (list? val) (= 1 (length val)))
        (car val)
        val)))
