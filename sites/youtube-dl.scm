;;; Copyright (c) 2014 Jesper Raftegard <jesper@huggpunkt.org>
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

(module youtube-dl (youtube-dl:uri->videos)
(import scheme chicken srfi-1 data-structures ports files
        miscmacros
        prelude stdouterr config platform misc-helpers json-parser network teve-http-client video)

(define (json-data->video data uri)
  (let* ((default-filename (json-ref data "_filename"))
         (title (json-ref data "title"))
         ;; youtube-dl populates a sublist with the key "formats" with
         ;; the data for each stream/encoding variant of a video,
         ;; while also putting the data of the automatically selected
         ;; format in the top-level. For videos where only one stream
         ;; is available, the "formats" sublist is not
         ;; created. Therefor, use the "formats" sublist for streams
         ;; data if it is available, and if it isn't, wrap top-level
         ;; in a list and pretend it's a "formats" sublist.
         (streams-data (or (json-ref data "formats") (list data)))
         (video-id (json-ref data "id"))
         (youtube-dl-extractor (json-ref data "extractor"))
         (add-video-values (lambda (stream)
                             (update-stream
                              stream
                              (make-stream-value 'title title)
                              (make-stream-value 'video-id video-id)
                              (make-stream-value 'view-at uri)
                              (make-stream-value 'youtube-dl-uri uri)
                              (make-stream-value 'youtube-dl-extractor youtube-dl-extractor)
                              (make-stream-value 'default-filename default-filename)))))
    (streams->video
     (map (lambda (d)
            (add-video-values
             (make-stream (make-stream-value 'default-extension (json-ref d "ext"))
                          (make-stream-value 'video-width (json-ref d "width"))
                          (make-stream-value 'video-height (json-ref d "height"))
                          (make-stream-value 'uri (json-ref d "url"))
                          (make-stream-value 'youtube-format-id (json-ref d "format_id"))
                          (make-stream-value 'stream-type 'youtube-dl))))
          streams-data))))

;;; XXX
;;; 1. Hardcoded paths! Hardcoded null output! Fix this, for Win too.
;;; 2. Is the program available at all? program-available? (?)
(define (get-json uri)
  (let ((json-file (create-temporary-file)))
    (dynamic-wind
      (lambda () #t)
      (lambda ()
        (system (stderr-to-null (conc (*cfg* 'external-programs 'youtube-dl)
                                      " -qj " uri " > " json-file)))
        (with-input-from-file json-file json-read->alist-tree))
      (lambda ()
      	(delete-file* json-file)))))

(define (youtube-dl:uri->videos uri)
  (and (program-available? (*cfg* 'external-programs 'youtube-dl))
       (let ((json-data (get-json uri)))
         (if (json-ref json-data)
             ;; Wrap in list, since we're expected to return a list of videos
             (list (json-data->video json-data uri))
             #f))))

)
