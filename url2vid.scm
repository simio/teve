(require-extension srfi-1)

;;; The scrape-list is a list of every url->video procedures.
(define scraper-list '())

;;; Allow "plugins" in sites/ to add procedures to the scraper-list
(define (add-scraper procedure)
  (set! scraper-list (cons procedure scraper-list)))

(include "sites/svt.scm")
(include "sites/tv4.scm")

;;; Dispatcher
;;; Each url->videos procedure is applied to the url. The resulting
;;; videos are appended into a video-list, which is returned.
(define (url->videos url)
  (delete-duplicates
   (apply append (filter-map (lambda (u->v) (u->v url)) scraper-list))))
