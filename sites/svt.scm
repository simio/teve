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

(require-extension srfi-1 srfi-2 srfi-12 srfi-13)
(require-extension miscmacros http-client)

(include "misc-helpers.scm")
(include "parsers/apple-hls.scm")
(include "parsers/json.scm")

(define (find-first-quoted-swf)
  (let ((ending (string-reverse ".swf")))
    (let find-first-swf ((stack "")
                         (is-discarding #t)
                         (ch (read-char)))
      (cond
       ((eof-object? ch) #f)
       ((and (char=? #\" ch) (string-prefix? ending stack))
        (let discard-until-eof ((ch (read-char)))
          (if (eof-object? ch)
              (string-reverse stack)
              (discard-until-eof (read-char)))))
       (else
        (find-first-swf (if is-discarding "" (conc ch stack))
                        (not-if (char=? #\" ch) is-discarding)
                        (read-char)))))))

(define (svt:swf-player-in url)
  (and-let* ((swf (with-input-from-string (fetch url)
                    find-first-quoted-swf)))
    (conc "http://www.svtplay.se" swf)))

(define (svt:stream-type-of url bitrate player-type)
  (let ((protocol (url->protocol url)))
    (cond
     ((string=? player-type "ios")
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
      (if (string-suffix? ".f4m" url)
          'hds
          'http))
     ((and (string=? player-type "wmv")
           (string=? protocol "mms"))
      'mms)
     (else
      #f))))

(define (svt:json-stream->streams jstr)
  (and-let* ((supplied-url (cdip (assoc "url" jstr)))
             (bitrate (cdip (assoc "bitrate" jstr)))
             (player-type (cdip (assoc "playerType" jstr)))
             (stream-type (svt:stream-type-of supplied-url bitrate player-type))
             (url (if (eq? stream-type 'hds)
                      (conc supplied-url "?hdcore=2.9.4&g="
                            (string-upcase (make-rnd-string 12)))
                      supplied-url)))
    (case stream-type
      ((hls)
       (hls-master->streams url))
      (else
       (list (make-stream (make-stream-value 'url url)
                          (make-stream-value 'stream-type stream-type)
                          (if (< 0 bitrate)
                              (make-stream-value 'bitrate bitrate))))))))

;;; Return a list of videos constructed from the json object
;;; downloaded from the supplied url, or return #f.
(define (svt:json-url->videos path)
  (and-let* ((uri (uri-reference path))
             (json-data (fetch path #:reader json-read->alist-tree))
             (references (json-ref json-data "video" "videoReferences")))
    (let* ((base-path (uri->base-path path))
           (subtitles (json-ref json-data "video" "subtitleReferences" 0 "url"))
           (video-id (json-ref json-data "videoId"))
           (popout-path (json-ref json-data "context" "popoutUrl"))
           (is-live (json-ref json-data "video" "live"))
           (play-path (cond
                      ((not popout-path) #f)
                      ((string-prefix? "http" popout-path) popout-path)
                      (else
                       (conc base-path popout-path))))
           (swf-player (delay (svt:swf-player-in play-path)))
           (add-video-values (lambda (stream)
                               (update-stream
                                stream
                                (make-stream-value 'default-filename
                                                   (conc "svt-video-" video-id))
                                (make-stream-value 'subtitles subtitles)
                                (make-stream-value 'view-at play-path)
                                (make-stream-value 'live is-live)
                                (if (eq? 'hls (stream-ref 'stream-type stream))
                                    (make-stream-value 'ffmpeg-parameters
                                                       "-bsf:a aac_adtstoasc"))
                                (if (eq? 'rtmp (stream-ref 'stream-type stream))
                                    (make-stream-value 'swf-player
                                                       (force swf-player)))))))
      (list
       (streams->video
        (fold (lambda (objs streams)
                (append (filter-map add-video-values objs)
                        streams))
              '()
              (filter-map svt:json-stream->streams references)))))))

;;; Return a JSON url extracted from an embedded svtplay video, or
;;; return #f.
(define (svt:embedded-player->json-url url)
  (and-let* ((source (fetch url))
             (value (first-html-attribute "data-json-href" source)))
    (string-replace-every "&amp;" "&" (uri-decode-string value))))

(define (svt:url->videos url)
  (cond
   ((or (string-contains url "://svtplay.se/")
        (string-contains url "://oppetarkiv.se/")
        (string-contains url "://öppetarkiv.se/")
        (string-contains url "://svt.se/"))
    (svt:url->videos (string-replace-every "://" "://www." url)))
   ((or (string-contains url "://www.svtplay.se/")
        (string-contains url "://www.oppetarkiv.se/")
        (string-contains url "://www.öppetarkiv.se/")
        (string-contains url "://xn--ppetarkiv-z7a.se/")
        (string-contains url "://www.xn--ppetarkiv-z7a.se/"))
    (filter video? (svt:json-url->videos
                    (add-http-get-query-var url "output" "json"))))
   ((or (string-contains url "://www.svt.se/"))
    (and-let* ((json-url (svt:embedded-player->json-url url)))
      (filter video?
              (svt:json-url->videos (if (string-prefix? "http" json-url)
                                        json-url
                                        (conc (uri->base-path url)
                                              json-url))))))
   (else
    #f)))

;;; Add svt:url->video to global scraper-table
(add-scraper svt:url->videos)
