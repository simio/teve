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
(define (stream-ref key stream)
  (let ((ret (assoc key stream)))
    (if ret (cdr ret) #f)))

(define (update-stream stream alist-of-new-values)
  (append alist-of-new-values
          (filter
           (lambda (pair)
             (and (pair? pair)
                  (let look-through ((keys (map car alist-of-new-values)))
                    (cond ((null? keys))
                          ((equal? (car keys) (car pair)) #f)
                          (else look-through (cdr keys))))))
           stream)))

(define (video-ref number video)
  (list-ref video number))

(define (make-stream alist-of-values)
  (update-stream '() (remove not alist-of-values)))

(define stream-length length)

(define (update-video video stream)
  (if stream
      (cons stream (remove (lambda (old-stream) (equal? old-stream stream)) video))
      video))

(define (make-video stream)
  (update-video '() (or stream '())))

(define video-length length)

;;; (pretty)-printers
(define (stream-printer stream)
  (let print-values ((rest stream)
                     (output ""))
    (if (null? rest)
        output
        (print-values (cdr rest)
                      (conc output
                            (string-pad-right (conc (caar rest) ":") 20)
                            (cdar rest) #\newline)))))

(define (video-printer video)
  (let print-streams ((rest video)
                      (output "")
                      (id 0))
    (if (null? rest)
        output
        (print-streams (cdr rest)
                       (conc output
                             (string-pad-right "stream id:" 20) id #\newline
                             (stream-printer (car rest)) #\newline)
                       (+ 1 id)))))

;;; Produce download(/playback/tee-playback) commands for the shell
(define (stream->download-command stream outfile)
  (case (stream-ref 'stream-type stream)
    ((hds)
     (conc "php AdobeHDS.php"
           " --manifest \"" (stream-ref 'url stream) "\""
           " --debug"
           " --outfile \"" outfile ".flv\""))
    ((hls)
     (conc "ffmpeg"
           " -i \"" (stream-ref 'url stream) "\""
           " -acodec copy -vcodec copy -absf aac_adtstoasc"
           " \"" outfile ".avi\""))
    ((rtmp)
     (conc "rtmpdump"
           " -r \"" (stream-ref 'url stream) "\""
           " -o \"" outfile ".flv\""
           " -W \"" (stream-ref 'swf-player stream) "\""))
    ((http wmv)
     (conc "curl"
           " -Lo \"" outfile (if (eqv? 'http (stream-ref 'stream-type stream))
                               ".flv"
                               ".wmv") "\""
           " \"" (stream-ref 'url stream) "\""))
    (else
     #f)))
