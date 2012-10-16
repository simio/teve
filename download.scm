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

(define (stream->curl/make-command stream outfile)
  (conc "curl \\" #\newline
        "    -L \\" #\newline
        "    -o " (shell-escape outfile) " \\" #\newline
        "    " (shell-escape (stream-ref 'url stream))))

(define (stream->rtmpdump/make-command stream outfile)
  (conc "rtmpdump \\" #\newline
        "    -r " (shell-escape (stream-ref 'url stream)) " \\" #\newline
        "    -o " (shell-escape outfile) " \\" #\newline
        (if (stream-ref 'swf-player stream)
            (conc "    -W " (shell-escape (stream-ref 'swf-player stream))
                  " \\" #\newline)
            "")
        (if (stream-ref 'live stream)
            (conc "    -v \\" #\newline)
            "")))

(define (stream->ffmpeg/make-command stream outfile)
  (conc "ffmpeg \\" #\newline
        "    -i " (shell-escape (stream-ref 'url stream)) " \\" #\newline
        "    -acodec copy -vcodec copy \\" #\newline
        (if (stream-ref 'ffmpeg-parameters stream)
            (conc "    " (stream-ref 'ffmpeg-parameters stream)
                  " \\" #\newline)
            "")
        (shell-escape outfile)))

(define (stream->adobehds.php/make-command stream outfile)
  (conc "php AdobeHDS.php \\" #\newline
        "    --manifest " (shell-escape (stream-ref 'url stream))
        " \\" #\newline
        (if (stream-ref 'adobe.hds-parameters stream)
            (conc "    " (stream-ref 'adobe.hds-parameters stream)
                  " \\" #\newline)
            "")
        "    --outfile " (shell-escape outfile)))

(define (stream->mplayer/make-command stream outfile)
  (conc "mplayer \\" #\newline
        "    -dumpstream -dumpfile " (shell-escape outfile)
        " \\" #\newline
        (if (stream-ref 'mplayer-parameters stream)
            (conc "    " (stream-ref 'mplayer-parameters stream)
                  " \\" #\newline)
            "")
        (shell-escape (stream-ref 'url stream))))

(define (stream->download-command stream outfile)
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
    (else #f)))
