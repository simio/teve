;;; Copyright (c) 2013 Jesper Raftegard <jesper@huggpunkt.org>
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

(require-extension miscmacros)

(include "video.scm")

;;; Calculate and return the distance between the supplied stream and
;;; the ideal size/bitrate.
(define (stream-distance stream reference-bitrate reference-width)
  (let* ((stream-width (if* (stream-ref 'video-width stream) it 0))
         (stream-bitrate (if* (stream-ref 'bitrate stream) it 0))
         (transport-weight-symbol (string->symbol
                                (conc "transport-weight-"
                                      (symbol->string
                                       (stream-ref 'stream-type stream)))))
         (transport-weight (if* (*cfg* 'preferences transport-weight-symbol)
                                it
                                1)))
    (* 0.5
       transport-weight
       ;; Here is the actual weight calculation
       (+ (abs (- reference-bitrate stream-bitrate))
          ;; The raw distances are multiplied by these weights
          ;; before added together and used for comparison.
          (* (/ (*cfg* 'preferences 'ideal-bitrate)
                (*cfg* 'preferences 'ideal-pixel-width))
             (abs (- reference-width stream-width)))))))

;;; Returns an alist with the following structure:
;;;
;;;     (distance . (stream-id . stream))
(define (video->stream-distance-table video)
  (sort
   (let calculate-distances ((distance-table '())
                             (rest (video->streams video))
                             (stream-id 0))
     (cond
      ((null? rest) distance-table)
      ((not (stream? (car rest)))
       (calculate-distances distance-table (cdr rest) (+ 1 stream-id)))
      (else
       (calculate-distances
        (cons
         (cons (stream-distance (car rest)
                                (*cfg* 'preferences 'ideal-bitrate)
                                (*cfg* 'preferences 'ideal-pixel-width))
               (cons stream-id (car rest)))
         distance-table)
        (cdr rest)
        (+ 1 stream-id)))))
   (lambda (a b)
     (< (car a) (car b)))))

(define (video->best-stream video)
  (cddar (video->stream-distance-table video)))

(define (video->best-stream-id video)
  (cadar (video->stream-distance-table video)))
