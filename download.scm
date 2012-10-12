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

(include "video.scm")

;;; Produce download(/playback/tee-playback) commands for the shell
(define (stream->download-command stream outfile)
  (case (stream-ref 'stream-type stream)
    ((hds)
     (conc "php AdobeHDS.php \\" #\newline
           "    --manifest " (shell-escape (stream-ref 'url stream))
           " \\" #\newline
           "    --debug --outfile " (shell-escape outfile) ".flv"))
    ((hls)
     (conc "ffmpeg \\" #\newline
           "    -i " (shell-escape (stream-ref 'url stream)) " \\" #\newline
           "    -acodec copy -vcodec copy -absf aac_adtstoasc \\" #\newline
           "    " (shell-escape outfile) ".avi"))
    ((rtmp)
     (conc "rtmpdump \\" #\newline
           "    -r " (shell-escape (stream-ref 'url stream)) " \\" #\newline
           "    -W " (shell-escape (stream-ref 'swf-player stream))
           " \\" #\newline
           (if (stream-ref 'live stream)
               (conc "    -v \\" #\newline)
               "")
           "    -o " (shell-escape outfile) ".flv"))
    ((http wmv)
     (conc "curl \\" #\newline
           "    -Lo " (shell-escape outfile)
           (if (eqv? 'http (stream-ref 'stream-type stream))
               ".flv"
               ".wmv") " \\" #\newline
               "    " (shell-escape (stream-ref 'url stream))))
    ((mms rtsp)
     (conc "mplayer \\" #\newline
           "    -dumpstream -dumpfile " (shell-escape outfile)
           (if (eqv? 'mms (stream-ref 'stream-type stream)) ".wmv" ".mp4")
           " \\" #\newline
           "    " (shell-escape (stream-ref 'url stream))))
    (else
     #f)))
