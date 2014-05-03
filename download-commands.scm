;;; Copyright (c) 2012, 2013, 2014 Jesper Raftegard <jesper@huggpunkt.org>
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

(module download-commands (downloadable?
                           stream->download-command
                           stream->play-command)
(import scheme chicken srfi-13 data-structures files utils
        miscmacros
        teve-http-client platform config video)

(define (stream->curl/make-command stream outfile)
  (conc (*cfg* 'external-programs 'curl)
        " -L"
        " -o " (qs outfile)
        " " (qs (stream-ref 'uri stream))))

(define (stream->rtmpdump/make-command stream outfile)
  (conc (*cfg* 'external-programs 'rtmpdump)
        " -r " (qs (stream-ref 'uri stream))
        (if* (stream-ref 'swf-path stream)
             (conc " -y" (qs it))
             "")
        " -o " (qs outfile)
        (if* (stream-ref 'swf-player stream)
             (conc " -W " (qs it))
             "")
        (if (stream-ref 'live stream)
            (conc " -v")
            "")))

(define (stream->ffmpeg/make-command stream outfile)
  (conc (*cfg* 'external-programs 'ffmpeg)
        " -i " (qs (stream-ref 'uri stream))
        " -codec copy"
        (if* (stream-ref 'ffmpeg-parameters stream)
             (conc " " it)
             "")
        " " (qs outfile)))

(define (stream->adobehds.php/make-command stream outfile)
  (conc (*cfg* 'external-programs 'php)
        " "
        (*cfg* 'external-programs 'adobehds.php)
        " --delete " ;; Script will clean up if not interrupted
        " --manifest " (qs (stream-ref 'uri stream))
        (if* (stream-ref 'adobe.hds-parameters stream)
             (conc " " it)
             "")
        " --outfile " (qs outfile)))

(define (stream->mplayer/make-command stream outfile)
  (conc (*cfg* 'external-programs 'mplayer)
        " -dumpstream -dumpfile " (qs outfile)
        (if* (stream-ref 'mplayer-parameters stream)
             (conc " " it)
             "")
        " " (qs (stream-ref 'uri stream))))

(define (stream->youtube-dl/make-command stream outfile)
  (conc (*cfg* 'external-programs 'youtube-dl)
        " -o " (qs outfile)
        " -f " (qs (stream-ref 'youtube-format-id stream))
        " " (qs (stream-ref 'youtube-dl-uri stream))))

(define (extract-uri-path str)
  (let ((uri (uri-reference str)))
    (if (not uri)
        str
        (apply conc (uri-path uri)))))

(define (stream->default-extension stream)
  (let ((swf-path-ext (and (eq? 'rtmp (stream-ref 'stream-type stream))
                           (pathname-extension
                            (extract-uri-path (stream-ref 'swf-path stream)))))
        (uri-path-ext (pathname-extension
                       (extract-uri-path (stream-ref 'uri stream)))))
    (cond
     ((stream-ref 'default-extension stream))
     (swf-path-ext)
     ((case (stream-ref 'stream-type stream)
        ((hls hds) "mp4")
        ((wmv mms) "wmv")
        (else #f)))
     (uri-path-ext)
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
   ((stream-ref 'uri stream)
    (pathname-file (stream-ref 'uri stream)))
   (else "downloaded-video")))

(define (stream->default-filename stream)
  (enforce-extension (stream->default-extension stream)
                     (stream->default-basename stream)))

;;; Ensures a valid filename for the supplied stream.
;;; Returns outfile (and enforces the correct file extension) if set.
;;; Returns a default filename otherwise.
(define (stream/filename?->output-filename stream filename?)
  (if filename?
      (enforce-extension (stream->default-extension stream) filename?)
      (stream->default-filename stream)))

(define (stream->download-command stream outfile)
  (let ((filename (stream/filename?->output-filename stream outfile)))
    (case (stream-ref 'stream-type stream)
      ((hds)
       (stream->adobehds.php/make-command stream filename))
      ((hls)
       (stream->ffmpeg/make-command stream filename))
      ((rtmp)
       (stream->rtmpdump/make-command stream filename))
      ((http wmv)
       (stream->curl/make-command stream filename))
      ((youtube-dl)
       (stream->youtube-dl/make-command stream filename))
      ((mms rtsp)
       (stream->mplayer/make-command stream filename))
      (else #f))))

;;; Tries to find out whether a stream or stream-type is downloadable on
;;; the current system, without invoking any external programs.
;;; If the supplied value is a stream, the stream-type will be derived from it.
;;; If the supplied value is a symbol, it is assumed to be a stream-type.
;;; If a stream-type can be derived as above, and it seems downloadable, return #t.
;;; Otherwise, return false.
(define (downloadable? o)
  (and-let* ((type (cond ((stream? o) (stream-ref 'stream-type o))
                         ((symbol? o) o)
                         (else #f))))
    (case type
      ((hds) (and (program-available? (*cfg* 'external-programs 'php))
                  (file-exists? (*cfg* 'external-programs 'adobehds.php))))
      ((hls) (program-available? (*cfg* 'external-programs 'ffmpeg)))
      ((rtmp) (program-available? (*cfg* 'external-programs 'rtmpdump)))
      ((http wmv) (program-available? (*cfg* 'external-programs 'curl)))
      ((mms rtsp) (program-available? (*cfg* 'external-programs 'mplayer)))
      ((youtube-dl) (program-available? (*cfg* 'external-programs 'youtube-dl)))
      (else #f))))
       
;;; Create a temporary fifo and return it's absolute name
(define (make-fifo extension)
  (create-temporary-file extension))

(define (player-command infile)
  (conc (*cfg* 'external-programs 'mplayer) " " infile))

(define (stream->play-command stream)
  (and-let* ((fifo (make-fifo (stream->default-extension stream))))
    (conc (stream->download-command stream fifo)
          "& \\" #\newline
          (player-command fifo))))

(define (stream->tee-play-command stream outfile)
  (let ((filename (stream/filename?->output-filename stream outfile)))
    (conc (stream->download-command stream filename)
        "& \\" #\newline
        (player-command filename))))

)
