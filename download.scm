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

(use srfi-13)
(require-extension miscmacros uri-generic)

(include "video.scm")

(define (stream->curl/make-command stream outfile)
  (conc "curl \\" #\newline
        "    -L \\" #\newline
        "    -o " (shell-escape outfile) " \\" #\newline
        "    " (shell-escape (stream-ref 'url stream))))

(define (stream->rtmpdump/make-command stream outfile)
  (conc "rtmpdump \\" #\newline
        "    -r " (shell-escape (stream-ref 'url stream)) " \\" #\newline
        (if* (stream-ref 'swf-path stream)
             (conc "    -y " (shell-escape it) " \\" #\newline)
             "")
        "    -o " (shell-escape outfile) " \\" #\newline
        (if* (stream-ref 'swf-player stream)
             (conc "    -W " (shell-escape it) " \\" #\newline)
             "")
        (if (stream-ref 'live stream)
            (conc "    -v \\" #\newline)
            "")))

(define (stream->ffmpeg/make-command stream outfile)
  (conc "ffmpeg \\" #\newline
        "    -i " (shell-escape (stream-ref 'url stream)) " \\" #\newline
        "    -acodec copy -vcodec copy \\" #\newline
        (if* (stream-ref 'ffmpeg-parameters stream)
             (conc "    " it " \\" #\newline)
             "")
        (shell-escape outfile)))

(define (stream->adobehds.php/make-command stream outfile)
  (conc "php AdobeHDS.php \\" #\newline
        "    --manifest " (shell-escape (stream-ref 'url stream))
        " \\" #\newline
        (if* (stream-ref 'adobe.hds-parameters stream)
             (conc "    " it " \\" #\newline)
             "")
        "    --outfile " (shell-escape outfile)))

(define (stream->mplayer/make-command stream outfile)
  (conc "mplayer \\" #\newline
        "    -dumpstream -dumpfile " (shell-escape outfile)
        " \\" #\newline
        (if* (stream-ref 'mplayer-parameters stream)
             (conc "    " it " \\" #\newline)
             "")
        (shell-escape (stream-ref 'url stream))))

(define (extract-uri-path str)
  (let ((uri (uri-reference str)))
    (if (not uri)
        str
        (apply conc (uri-path uri)))))

(define (stream->default-extension stream)
  (let ((swf-path-ext (and (eq? 'rtmp (stream-ref 'stream-type stream))
                           (pathname-extension
                            (extract-uri-path (stream-ref 'swf-path stream)))))
        (url-path-ext (pathname-extension
                       (extract-uri-path (stream-ref 'url stream)))))
    (cond
     ((stream-ref 'filename-extension stream))
     (swf-path-ext)
     ((case (stream-ref 'stream-type stream)
        ((hls hds) "mp4")
        ((wmv mms) "wmv")
        (else #f)))
     (url-path-ext)
     (else "flv"))))

(define (enforce-extension extension filename)
  (if (string-suffix? extension filename)
      filename
      (conc filename "." extension)))

(define (stream->default-basename stream)
  (cond
   ((stream-ref 'default-filename stream))
   ((stream-ref 'swf-path stream)
    (pathname-file (stream-ref 'swf-path stream)))
   ((stream-ref 'url stream)
    (pathname-file (stream-ref 'url stream)))
   (else "downloaded-video")))

(define (stream->default-filename stream)
  (enforce-extension (stream->default-extension stream)
                     (stream->default-basename stream)))

(define (stream->download-command stream outfile)
  (let ((filename (if outfile outfile (stream->default-filename stream))))
    (case (stream-ref 'stream-type stream)
      ((hds)
       (stream->adobehds.php/make-command stream outfile))
      ((hls)
       (stream->ffmpeg/make-command stream outfile))
      ((rtmp)
       (stream->rtmpdump/make-command stream outfile))
      ((http wmv)
       (stream->curl/make-command stream outfile))
      ((mms rtsp)
       (stream->mplayer/make-command stream outfile))
      (else #f))))

;;; Create a temporary fifo and return it's absolute name
(define (create-fifo extension)
  (create-temporary-file extension))

(define (player-command infile)
  (conc "mplayer " infile))

(define (stream->play-command stream)
  (and-let* ((fifo (create-fifo (stream->default-extension stream))))
    (conc (stream->download-command stream fifo)
          "& \\" #\newline
          (player-command fifo))))

(define (stream->tee-play-command stream outfile)
  (conc (stream->download-command stream outfile)
        "& \\" #\newline
        (player-command outfile)))