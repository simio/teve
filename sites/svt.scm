;;; Copyright (c) 2012, 2013 Jesper Raftegard <jesper@huggpunkt.org>
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

(module svt (svt:uri->videos)
(import scheme chicken srfi-1 srfi-13 ports data-structures
        miscmacros
        prelude misc-helpers teve-http-client network json-parser apple-hls-parser video)

(define (find-first-quoted-swf)
  (let ((ending (string-reverse ".swf")))
    (let find-first-swf ((stack "")
                         (is-discarding #t)
                         (ch (read-char)))
      (cond ((eof-object? ch) #f)
            ((and (char=? #\" ch) (string-prefix? ending stack))
             (let discard-until-eof ((ch (read-char)))
               (if (eof-object? ch)
                   (string-reverse stack)
                   (discard-until-eof (read-char)))))
            (else
             (find-first-swf (if is-discarding "" (conc ch stack))
                             (not-if (char=? #\" ch) is-discarding)
                             (read-char)))))))

(define (svt:swf-player-in uri)
  (and-let* ((swf (with-input-from-string (fetch uri)
                    find-first-quoted-swf)))
    (conc "http://www.svtplay.se" swf)))

(define (svt:stream-type-of uri bitrate player-type)
  (let ((protocol (uri->protocol uri)))
    (cond ((string=? player-type "ios")
           'hls)
          ((and (string=? player-type "wmv")
                (string-prefix? "http" protocol))
           'http)
          ((and (string=? player-type "mpeg4")
                (string=? protocol "rtsp"))
           'rtsp)
          ((and (string=? player-type "flash")
                (string-prefix? "rtmp" protocol))
           'rtmp)
          ((and (string=? player-type "flash")
                (string-prefix? "http" protocol))
           (if (string-suffix? ".f4m" uri)
               'hds
               'http))
          ((and (string=? player-type "wmv")
                (string=? protocol "mms"))
           'mms)
          (else #f))))

(define (svt:json-stream->streams jstr)
  (and-let* ((supplied-uri (cdip (assoc "url" jstr)))
             (bitrate (cdip (assoc "bitrate" jstr)))
             (player-type (cdip (assoc "playerType" jstr)))
             (stream-type (svt:stream-type-of supplied-uri bitrate player-type))
             (uri (if (eq? stream-type 'hds)
                      (conc supplied-uri "?hdcore=2.9.4&g=" (string-upcase (make-rnd-string 12)))
                      supplied-uri)))
    (case stream-type
      ((hls) (hls-master->streams uri))
      (else (list (make-stream (make-stream-value 'uri uri)
                               (make-stream-value 'stream-type stream-type)
                               (if (< 0 bitrate)
                                   (make-stream-value 'bitrate bitrate))))))))

;;; Return a list of videos constructed from the json object
;;; downloaded from the supplied uri, or return #f.
(define (svt:json-uri->videos path)
  (and-let* ((uri (uri-reference path))
             (json-data (fetch path #:reader json-read->alist-tree))
             (references (json-ref json-data "video" "videoReferences")))
    (let* ((base-path (uri->base-path path))
           (subtitles (json-ref json-data "video" "subtitleReferences" 0 "url"))
           (video-id (json-ref json-data "videoId"))
           (popout-path (json-ref json-data "context" "popoutUrl"))
           (is-live (json-ref json-data "video" "live"))
           (play-path (cond ((not popout-path) #f)
                            ((string-prefix? "http" popout-path) popout-path)
                            (else (conc base-path popout-path))))
           (swf-player (delay (svt:swf-player-in play-path)))
           (title (json-ref json-data "context" "title"))
           (suggest-filename (lambda () (or title (conc "svt-video-" video-id))))
           (add-video-values (lambda (stream)
                               (update-stream
                                stream
                                (make-stream-value 'title title)
                                (make-stream-value 'default-filename (suggest-filename))
                                (make-stream-value 'subtitles subtitles)
                                (make-stream-value 'view-at play-path)
                                (make-stream-value 'live is-live)
                                (if (eq? 'hls (stream-ref 'stream-type stream))
                                    (make-stream-value 'ffmpeg-parameters "-bsf:a aac_adtstoasc"))
                                (if (eq? 'rtmp (stream-ref 'stream-type stream))
                                    (make-stream-value 'swf-player (force swf-player)))))))
      (list (streams->video (fold (lambda (objs streams)
                                    (append (filter-map add-video-values objs) streams))
                                  '()
                                  (filter-map svt:json-stream->streams references)))))))

;;; Return a JSON uri extracted from an embedded svtplay video, or
;;; return #f.
(define (svt:embedded-player->json-uri uri)
  (and-let* ((source (fetch uri))
             (value (first-html-attribute "data-json-href" source)))
    (string-replace-every "&amp;" "&" (uri-decode-string value))))

(define (svt:uri->videos uri)
  (cond
   ((or (string-contains uri "://svtplay.se/")
        (string-contains uri "://oppetarkiv.se/")
        (string-contains uri "://öppetarkiv.se/")
        (string-contains uri "://svt.se/"))
    (svt:uri->videos (string-replace-every "://" "://www." uri)))
   ((or (string-contains uri "://www.svtplay.se/")
        (string-contains uri "://www.oppetarkiv.se/")
        (string-contains uri "://www.öppetarkiv.se/")
        (string-contains uri "://xn--ppetarkiv-z7a.se/")
        (string-contains uri "://www.xn--ppetarkiv-z7a.se/"))
    (and-let* ((result (svt:json-uri->videos (add-http-get-query-var uri "output" "json"))))
      (filter video? result)))
   ((or (string-contains uri "://www.svt.se/"))
    (and-let* ((json-uri (svt:embedded-player->json-uri uri))
               (result (svt:json-uri->videos (if (string-prefix? "http" json-uri)
                                                 json-uri
                                                 (conc (uri->base-path uri) json-uri)))))
      (filter video? result)))
   (else #f)))

)
