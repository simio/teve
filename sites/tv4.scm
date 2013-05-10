#| Copyright (c) 2012, 2013 Jesper Raftegard <jesper@huggpunkt.org>
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

(include "misc-helpers.scm")
(include "video.scm")

(define (tv4:download-xml-data xml-base-url)
  (and-let* ((xml-base (force (download-xml xml-base-url)))
             (xml-play (force (download-xml (conc xml-base-url "/play")))))
    (list (caddr xml-base)
          (caddr xml-play))))

(define (tv4:xml-item->stream data)
  (and-let* ((url-form (sxml-ref data 'base))
             (url (sxml-ref data 'url))
             (scheme (or (sxml-ref data 'scheme)
                         (url->protocol (sxml-ref data 'url))))
             (mediaformat (sxml-ref data 'mediaFormat))
             (is-media-stream? (not (string=? "smi" mediaformat)))
             (bitrate (string->number (sxml-ref data 'bitrate))))
    (cond
     ((string=? mediaformat "flv")
      ;; Was/is this the correct value? Are there even any rtmp streams
      ;; left at TV4 Play?
      (make-stream (make-stream-value 'stream-type 'rtmp)
                   (make-stream-value 'url url-form)
                   (make-stream-value 'swf-path url)
                   (make-stream-value 'bitrate bitrate)))
     ((string=? mediaformat "mp4")
      (make-stream (make-stream-value 'stream-type 'hds)
                   (make-stream-value 'url url)
                   (make-stream-value 'bitrate bitrate)))
     (else #f))))

(define (tv4:xml-items->subtitles-url data)
  (cond
   ((or (null? data)
        (not (string? (sxml-ref (car data) 'mediaFormat))))
    #f)
   ((string=? "smi" (sxml-ref (car data) 'mediaFormat))
    (sxml-ref (car data) 'url))
   (else
    (tv4:xml-items->subtitles-url (cdr data)))))

(define (tv4:xml-data->video data)
  (and-let* ((xml-items (sxml-ref/proper data 'playback 'items)))
    (let* ((subtitles (tv4:xml-items->subtitles-url xml-items))
           (is-live (string=? "true" (sxml-ref data 'playback 'live)))
           (video-id (sxml-ref data 'playback '@ 'assetId))
           (suggest-filename (lambda ()
                               (if* (sxml-ref data 'playback 'title)
                                    it
                                    (conc "tv4-video-" video-id))))
           (swf-player "http://www.tv4play.se/flash/tv4playflashlets.swf"))
      (streams->video
       (map (lambda (stream)
              (update-stream stream
                             (if subtitles
                                 (make-stream-value 'subtitles subtitles))
                             (make-stream-value 'live is-live)
                             (make-stream-value 'default-filename
                                                (suggest-filename))
                             (if (eq? 'rtmp (stream-ref 'stream-type stream))
                                 (make-stream-value 'swf-player swf-player))))
            (filter-map tv4:xml-item->stream xml-items))))))

(define (tv4:xml-url->video xml-url)
  (and-let* ((data (tv4:download-xml-data xml-url)))
    (tv4:xml-data->video data)))

(define (tv4:video-id->xml-url id)
  (conc "http://prima.tv4play.se/api/web/asset/" id))

(define (tv4:tv4play-url->xml-url url)
  (and-let* ((url-obj (uri-reference url))
             (query (uri-query url-obj))
             (video-id (cdip (assq 'video_id query)))
             (xml-base-url (tv4:video-id->xml-url video-id)))
    xml-base-url))

(define (tv4:embedded-video->xml-url url)
  (and-let* ((source (force (delay-download url)))
             (raw-flash-vars (first-html-attribute "data-flash-vars"
                                                   source
                                                   #\'))
             (flash-vars (with-input-from-string
                             raw-flash-vars
                           json-read-and-sanitise))
             (raw-video-id (cdip (assoc "vid" flash-vars)))
             (video-id (string->number raw-video-id)))
    (tv4:video-id->xml-url video-id)))

(define (tv4:url->videos url)
  (cond
   ((or (string-contains url "://www.tv4play.se/")
        (string-contains url "://tv4play.se/"))
    (filter video?
            (list (tv4:xml-url->video (tv4:tv4play-url->xml-url url)))))
   ((or (string-contains url "://www.tv4.se/")
        (string-contains url "://tv4.se/"))
    (filter video?
            (list (tv4:xml-url->video (tv4:embedded-video->xml-url url)))))
   (else
    #f)))

(add-scraper tv4:url->videos)
