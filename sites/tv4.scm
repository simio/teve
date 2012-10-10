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

(require-extension miscmacros uri-common)

(include "misc-helpers.scm")

(define (tv4:download-xml-data xml-base-url)
  (and-let* ((xml-base (download-xml xml-base-url))
             (xml-play (download-xml (conc xml-base-url "/play"))))
    (list (caddr xml-base)
          (caddr xml-play))))

(define (tv4:xml-items->stream data)
  (and-let* ((url (sxml-ref data 'base))
             (swf-path (sxml-ref data 'url))
             (scheme (sxml-ref data 'scheme))
             (mediaformat (sxml-ref data 'mediaFormat))
             (is-media-stream? (not (string=? "smi" mediaformat)))
             (bitrate (string->number (sxml-ref data 'bitrate))))
    (make-stream (make-stream-value 'stream-type 'rtmp)
                 (make-stream-value 'url url)
                 (make-stream-value 'swf-path swf-path)
                 (make-stream-value 'bitrate bitrate))))

(define (tv4:xml-items->subtitles-url data)
  (cond
   ((null? data) #f)
   ((string=? "smi" (sxml-ref (car data) 'mediaFormat))
    (sxml-ref (car data) 'url))
   (else
    (tv4:xml-items->subtitles-url (cdr data)))))
   

(define (tv4:xml-data->video data)
  (and-let* ((xml-items (sxml-ref data 'playback 'items)))
    (let* ((subtitles (tv4:xml-items->subtitles-url xml-items))
           (is-live (string=? "true" (sxml-ref data 'playback 'live)))
           (swf-player "http://www.tv4play.se/flash/tv4playflashlets.swf"))
      (streams->video
       (map (lambda (stream)
              (update-stream stream
                             (if subtitles
                                 (make-stream-value 'subtitles subtitles))
                             (if is-live
                                 (make-stream-value 'live #t))
                             (if (eq? 'rtmp (stream-ref 'stream-type stream))
                                 (make-stream-value 'swf-player swf-player))))
            (filter-map tv4:xml-items->stream xml-items))))))
         
(define (tv4:xml-url->video xml-url)
  (and-let* ((data (tv4:download-xml-data xml-url)))
    (tv4:xml-data->video data)))

(define (tv4:tv4play-url->xml-url url)
  (and-let* ((url-obj (uri-reference url))
             (query (uri-query url-obj))
             (video-id (cdip (assq 'videoid query)))
             (xml-base-url (conc "http://prima.tv4play.se/api/web/asset/"
                                 video-id)))
    xml-base-url))

(define (tv4:url->videos url)
  (cond
   ((or (string-contains url "://www.tv4play.se/")
        (string-contains url "://tv4play.se/"))
    (list (tv4:xml-url->video (tv4:tv4play-url->xml-url url))))
   ((or (string-contains url "://www.tv4.se/")
        (string-contains url "://tv4.se/"))
    #f) ;XXX not yet implemented
   (else
    #f)))

(add-scraper tv4:url->videos)