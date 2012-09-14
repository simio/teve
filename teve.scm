;;; Copyright (c) 2012 Jesper Raftegard <jesper@huggpunkt.org>
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

(use utf8)

;; TODO: Get rid of these, e.g. move them to where they are used.
;; Also, separate the builtins from the chicken-install:ed.
(require-extension srfi-37 json http-client vector-lib irregex)

(define program-name "teve")
(define program-filename program-name)
(define program-version "prototype-1")

(define verbosity #f)
(define action 'nothing)
(define id -1)
(define outfile "movie")

(include "talk-is-cheap.scm")
(include "misc-helpers.scm")
(include "parse-flags.scm")
(include "video.scm")
(include "svt.scm")

;;; Do something
(let ((command-line-args (parse-flags)))
  (if (not (null? command-line-args))
      (let* ((url (car command-line-args))
             (video (url->video url)))
        (case action
          ((list)
           (print* (video-printer video)))
          ((download)
           (if (<= 0 id (length video))
               (let ((download-command (video->download-command (list-ref video id) outfile)))
                 (if download-command
                     (print "$ " download-command)
                     (print "Unknown stream type: #" id)))
               (print "Could not find stream #" id "." #\newline
                      "Please verify id from output of '" program-filename " -l " url "'")))
          (else
           (print "Pardon?"))))))
(exit 0)
