#| Copyright (c) 2013 Jesper Raftegard <jesper@huggpunkt.org>
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

(require-extension miscmacros)

(include "video.scm")

;;; Wanted bitrate/pixel width. A distance from these values is
;;; calculated for each stream, and the nearest stream is selected by
;;; the algorithm. (A wanted-height would be pointless, since the
;;; width is a good enough indicator of the video resolution.)
;;;
;;; These should later be specified in a dotfile and overridable from
;;; the CLI.

(define wanted-bitrate 2500)
(define wanted-width 1280)

;;; The raw distances are multiplied by these weights before added
;;; together and used for comparison.
(define bitrate-distance-weight 1)
(define width-distance-weight (/ wanted-bitrate wanted-width))

;;; Calculate and return the distance between the supplied stream and
;;; the ideal size/bitrate.
(define (stream-distance stream reference-bitrate reference-width)
  (let ((stream-width (if* (stream-ref 'video-width stream)
                           it
                           0))
        (stream-bitrate (if* (stream-ref 'bitrate stream)
                             it
                             0)))
    (/ (+
        (* bitrate-distance-weight
           (abs (- reference-bitrate stream-bitrate)))
        (* width-distance-weight
           (abs (- reference-width stream-width))))
       2)))

;;; Returns an alist with total distance for keys and streams for values.
(define (video->stream-distance-table video)
  (sort
   (let calculate-distances ((distance-table '())
                             (rest (video->streams video)))
     (cond
      ((null? rest) distance-table)
      ((not (stream? (car rest)))
       (calculate-distances distance-table (cdr rest)))
      (else
       (calculate-distances
        (cons (cons (stream-distance (car rest) wanted-bitrate wanted-width)
                    (car rest))
              distance-table)
        (cdr rest)))))
   (lambda (a b)
     (< (car a) (car b)))))
