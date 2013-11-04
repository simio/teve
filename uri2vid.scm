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

(module uri->video (uri->videos)
(import scheme chicken srfi-1 data-structures
        misc-helpers config svt tv4)

;;; The scrape-list is a list of all uri->video procedures
(define *scraper-list* '())

;;; Allow "plugins" in sites/ to add procedures to the scraper-list
(define (add-scraper procedure)
  (set! *scraper-list* (cons procedure *scraper-list*)))

(define (prepend-default-protocol uri)
  (if (uri->protocol uri)
      uri
      (conc "http://" uri)))
      
;;; Add svt:uri->video to global scraper-table
(add-scraper svt:uri->videos)
(add-scraper tv4:uri->videos)

;;; Expand preset strings into uri:s
(define (expand-channels str)
  (or (*cfg* 'channels str) str))

;;; Dispatcher
;;; Each uri->videos procedure is applied to the uri. The resulting
;;; videos are appended into a video-list, which is returned.
(define (uri->videos uri)
  (delete-duplicates
   (apply append (filter-map (lambda (u->v)
                               (u->v (prepend-default-protocol (expand-channels uri))))
                             *scraper-list*))))

)
