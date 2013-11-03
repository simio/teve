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

(module tv4 (tv4:uri->videos)
(import scheme chicken srfi-1 srfi-13 data-structures ports
        miscmacros
        prelude misc-helpers apple-hls-parser json-parser network teve-http-client video)

(define (tv4:download-xml-data xml-base-uri)
  (and-let* ((xml-base (fetch xml-base-uri #:reader xml-read))
             (xml-play-hds (fetch (conc xml-base-uri "/play") #:reader xml-read))
             (xml-play-hls (fetch (conc xml-base-uri "/play?protocol=hls") #:reader xml-read)))
    (list (caddr xml-base)
          (caddr xml-play-hds)
          (caddr xml-play-hls))))

(define (tv4:xml-item->streams data)
  (and-let* ((uri-form (sxml-ref data 'base))
             (uri (sxml-ref data 'url))
             (scheme (or (sxml-ref data 'scheme)
                         (uri->protocol (sxml-ref data 'url))))
             (mediaformat (sxml-ref data 'mediaFormat))
             (is-media-stream? (not (string=? "smi" mediaformat)))
             (bitrate (string->number (sxml-ref data 'bitrate)))
             (add-hls-values (lambda (stream)
                               (update-stream
                                stream
                                (make-stream-value 'stream-type 'hls)
                                (make-stream-value 'ffmpeg-parameters "-absf aac_adtstoasc")
                                (make-stream-value 'uri uri)))))
    (cond
     ((string=? mediaformat "flv")
      ;; Was/is this the correct value? Are there even any rtmp streams
      ;; left at TV4 Play?
      (list (make-stream (make-stream-value 'stream-type 'rtmp)
                         (make-stream-value 'uri uri-form)
                         (make-stream-value 'swf-path uri)
                         (make-stream-value 'bitrate bitrate))))
     ((and (string=? mediaformat "mp4")
           (string-suffix? ".m3u8" uri-form))
      (map add-hls-values (hls-master->streams uri)))
     ((and (string=? mediaformat "mp4")
           (string-suffix? ".f4m" uri-form))
      (list (make-stream (make-stream-value 'stream-type 'hds)
                         (make-stream-value 'uri uri)
                         (make-stream-value 'bitrate bitrate))))
     (else #f))))

(define (tv4:xml-items->subtitles-uri data)
  (cond ((or (null? data)
             (not (string? (sxml-ref (car data) 'mediaFormat))))
         #f)
        ((string=? "smi" (sxml-ref (car data) 'mediaFormat))
         (sxml-ref (car data) 'url))
        (else
         (tv4:xml-items->subtitles-uri (cdr data)))))

(define (tv4:xml-data->video data)
  (and-let* ((playback-items (filter-map (lambda (x)
                                           (if (equal? (caip x) 'playback)
                                               x
                                               #f))
                                         data)))
    (video-join (filter-map
     (lambda (playback-item)
       (and-let* ((xml-items (sxml-ref/proper playback-item 'items)))
         (let* ((subtitles (tv4:xml-items->subtitles-uri xml-items))
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
                                  (make-stream-value 'title (sxml-ref data 'playback 'title))
                                  (make-stream-value 'default-filename (suggest-filename))
                                  (if (eq? 'rtmp (stream-ref 'stream-type stream))
                                      (make-stream-value 'swf-player swf-player))))
                 (join (filter-map tv4:xml-item->streams xml-items)))))))
     playback-items))))

(define (tv4:xml-uri->video xml-uri)
  (and-let* ((data (tv4:download-xml-data xml-uri)))
    (tv4:xml-data->video data)))

(define (tv4:video-id->xml-uri id)
  (conc "http://prima.tv4play.se/api/web/asset/" id))

(define (tv4:tv4play-uri->xml-uri uri)
  (and-let* ((uri-obj (uri-reference uri))
             (query (uri-query uri-obj))
             (video-id (cdip (assq 'video_id query)))
             (xml-base-uri (tv4:video-id->xml-uri video-id)))
    xml-base-uri))

(define (tv4:embedded-video->xml-uri uri)
  (and-let* ((source (fetch uri))
             (raw-flash-vars (first-html-attribute "data-flash-vars" source #\'))
             (flash-vars (with-input-from-string raw-flash-vars json-read->alist-tree))
             (raw-video-id (cdip (assoc "vid" flash-vars)))
             (video-id (string->number raw-video-id)))
    (tv4:video-id->xml-uri video-id)))

(define (tv4:uri->videos uri)
  (cond
   ((or (string-contains uri "://www.tv4play.se/")
        (string-contains uri "://tv4play.se/"))
    (filter video? (list (tv4:xml-uri->video (tv4:tv4play-uri->xml-uri uri)))))
   ((or (string-contains uri "://www.tv4.se/")
        (string-contains uri "://tv4.se/"))
    (filter video? (list (tv4:xml-uri->video (tv4:embedded-video->xml-uri uri)))))
   (else
    #f)))

)
