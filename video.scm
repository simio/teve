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

(require-extension srfi-1 srfi-13)

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
  (let ((ret (assoc key stream)))
    (if ret (cdr ret) #f)))

(define (stream-value? obj)
  (and (pair? obj)
       (symbol? (car obj))))

(define (make-stream-value key val)
  (cons key val))

(define (stream? obj)
  (and (list? obj)
       (every stream-value? obj)))

(define (update-stream stream . values)
  (delete-duplicates
   (append (filter stream-value? values) stream)
   (lambda (o1 o2) (equal? o1 o2))))
  
(define (make-stream . values)
  (apply update-stream (cons '() (filter stream-value? values))))

(define stream-length length)

(define (video-ref number video)
  (list-ref video number))

(define (video? obj)
  (and (list? obj)
       (every stream? obj)))

(define (update-video video . streams)
  (delete-duplicates (append (filter stream? streams) video)))

(define (make-video . streams)
  (apply update-video (cons '() (filter stream? streams))))

(define video-length length)

;;; (pretty)-printers
(define (stream-printer stream)
  (let print-values ((rest stream)
                     (output '()))
    (if (null? rest)
        (apply conc (reverse output))
        (print-values (cdr rest)
                      (fold cons output
                            (list (string-pad-right (conc (caar rest) ":") 20)
                                  (cdar rest) #\newline))))))

(define (video-printer video)
  (let print-streams ((rest video)
                      (output '())
                      (id 0))
    (if (null? rest)
        (apply conc (reverse output))
        (print-streams (cdr rest)
                       (fold cons output
                             (list
                              (string-pad-right "stream id:" 20) id #\newline
                              (stream-printer (car rest)) #\newline))
                             (+ 1 id)))))

;;; Produce download(/playback/tee-playback) commands for the shell
(define (stream->download-command stream outfile)
  (case (stream-ref 'stream-type stream)
    ((hds)
     (conc "php AdobeHDS.php \\" #\newline
           "    --manifest " (stream-ref 'url stream) " \\" #\newline
           "    --debug --outfile " outfile ".flv"))
    ((hls)
     (conc "ffmpeg \\" #\newline
           "    -i " (stream-ref 'url stream) " \\" #\newline
           "    -acodec copy -vcodec copy -absf aac_adtstoasc \\" #\newline
           "    " outfile ".avi"))
    ((rtmp)
     (conc "rtmpdump \\" #\newline
           "    -r " (stream-ref 'url stream) " \\" #\newline
           "    -W " (stream-ref 'swf-player stream) " \\" #\newline
           "    -o " outfile ".flv"))
    ((http wmv)
     (conc "curl \\" #\newline
           "    -Lo " outfile (if (eqv? 'http
                                        (stream-ref 'stream-type stream))
                                  ".flv"
                                  ".wmv") " \\" #\newline
                                  "    " (stream-ref 'url stream)))
    ((mms rtsp)
     (conc "mplayer \\" #\newline
           "    -dumpstream -dumpfile " outfile
           (if (eqv? 'mms (stream-ref 'stream-type stream)) ".wmv" ".mp4")
           " \\" #\newline
           "    " (stream-ref 'url stream)))
    (else
     #f)))
