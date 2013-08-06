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

(require-extension srfi-1 srfi-13)

(include "misc-helpers.scm")

(include "printers/scheme.scm")
(include "printers/json.scm")

;;; Stream/video makers, accessors and updaters
;;; An object is a valid video iff it is a list where every item is a stream.
;;; An object is a valid stream iff it is an alist and the car of every
;;; item is a symbol.
;;;
;;; Do not rely on lists of streams being valid videos outside of this file.
;;;
;;; Streams may not contain the values #t or #f, since #f is returned by
;;; stream-ref to signify the specified key does not exist.

(define (stream-ref key stream)
  (and-let* ((ret (if (list? stream)
                      (assoc key stream)
                      #f)))
    (cdr ret)))

(define (stream-value? obj)
  (and (pair? obj)
       (symbol? (car obj))))

(define (make-stream-value key val)
  (if (symbol? key)
      (cons key val)
      #f))

(define (stream? obj)
  (and (not (null? obj))
       (list? obj)
       (every stream-value? obj)))

(define (update-stream stream . values)
  (if (or (stream? stream) (null? stream))
      (delete-duplicates
       (append (filter stream-value? values) stream)
       (lambda (o1 o2) (equal? (car o1) (car o2))))
      #f))

;;; Pass one or more pairs as args
(define (make-stream . values)
  (apply update-stream (cons '() (filter stream-value? values))))

;;; Allow passing all values in a single list of values
(define-syntax pairs->stream
  (syntax-rules ()
    ((pairs->stream pairs)
     (apply make-stream pairs))))

(define stream-length length)

(define (video-ref number video)
  (and-let* ((ret (if (and (video? video)
                           (< -1 number (video-length video)))
                      (list-ref video number)
                      #f)))
    ret))

(define (video? obj)
  (and (not (null? obj))
       (list? obj)
       (every stream? obj)))

(define (update-video video . streams)
  (if (or (video? video) (null? video))
      (delete-duplicates
       (append (filter stream? streams) video))
      #f))

;;; Pass one or more streams as args
(define (make-video . streams)
  (apply update-video (cons '() (filter stream? streams))))

;;; Allow passing all streams in a single list of streams
(define-syntax streams->video
  (syntax-rules ()
    ((streams->video streams)
     (apply make-video streams))))

;;; 
(define (video->streams video)
  video)

(define video-length length)

(define video-join join)

;;; (pretty)-printers
(define (stream-printer stream)
  (let print-values ((rest stream)
                     (output '()))
    (if (null? rest)
        (apply conc (reverse output))
        (print-values (cdr rest)
                      (fold cons output
                            (list (string-pad-right (conc (caar rest) ":") 22)
                                  (cdar rest) #\newline))))))

(define (video-printer video)
  (case (*cfg* 'operators 'machine-output)
    ((scheme) (scheme-printer video))
    ((json) (json-printer video))
    (else
     (let print-streams ((rest video)
                         (output '())
                         (id 0))
       (if (null? rest)
           (apply conc (reverse output))
           (print-streams (cdr rest)
                          (fold cons output
                                (list
                                 (string-pad-right "stream id:" 22) id #\newline
                                 (stream-printer (car rest)) #\newline))
                          (+ 1 id)))))))
