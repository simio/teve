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

(require-extension srfi-1)

(include "misc-helpers.scm")

;;; The scrape-list is a list of all url->video procedures
(define *scraper-list* '())

;;; Allow "plugins" in sites/ to add procedures to the scraper-list
(define (add-scraper procedure)
  (set! *scraper-list* (cons procedure *scraper-list*)))

(define (prepend-default-protocol url)
  (if (url->protocol url)
      url
      (conc "http://" url)))
      

(include "sites/svt.scm")
(include "sites/tv4.scm")

;;; Dispatcher
;;; Each url->videos procedure is applied to the url. The resulting
;;; videos are appended into a video-list, which is returned.
(define (url->videos url)
  (delete-duplicates
   (apply append
     (filter-map (lambda (u->v) (u->v (prepend-default-protocol url)))
                 *scraper-list*))))
