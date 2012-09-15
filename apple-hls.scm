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

;;; These functions actually don't parse HLS playlists. All they
;;; do is pretend.

(require-extension http-client)

(include "intarweb-hack.scm")
(include "misc-helpers.scm")
(include "video.scm")

(define (hls:parse-playlist str)
  (define (read-stream mesh slat)
    (and-let* ((pairs (varlist->alist mesh))
               (resolution (assoc "RESOLUTION" pairs))
               (bandwidth (assoc "BANDWIDTH" pairs)))
              (make-stream
               (list (cons 'resolution
                           (or (x-sep-resolution->pair (cdr resolution))
                               (cdr resolution)))
                     (cons 'bitrate (/ (cdr bandwidth) 1000))
                     (cons 'url (uri-decode-string (car slat)))))))
  (let read-entries ((playlist (cdr (string-split str (string #\newline))))
                     (video '()))
    (cond ((or (null? playlist)
               (null? (cdr playlist)))
           video)
          (else (read-entries (cddr playlist)
                              (or (update-video
                                   video
                                   (read-stream (car playlist)
                                                (cdr playlist)))
                                  video))))))

(define (hls-master->video playlist-url)
  (let ((playlist (with-input-from-request (make-emo-request playlist-url)
                                           #f
                                           read-string)))
    (if (not (and (string? playlist)
                  (string-contains playlist (string #\newline))))
        #f
        (hls:parse-playlist playlist))))