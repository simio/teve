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

(define program-display-name "teve")
(define program-version "0.3-devel")

(require-extension miscmacros ini-file ssax http-client sha2 message-digest json srfi-1 srfi-13 srfi-18)

;; Ordering matters here
(include "http-client.scm")   ; reexports uri-common, intarweb and http-client
(include "scheme-prelude/stdouterr.scm")
(include "scheme-prelude/prelude.scm")
(include "platform.scm")
(include "misc-helpers.scm")
(include "dot-locking.scm")
(include "config.scm")
(include "network.scm")

;; This ordering is particularly ugly
(include "parsers/json.scm")
(include "video.scm")
(include "parsers/apple-hls.scm")

;; This should be automated
(include "sites/svt.scm")
(include "sites/tv4.scm")

;; This should be renamed
(include "uri2vid.scm")

;; This goofy row will go away soon enough.
(import teve-http-client stdouterr prelude platform misc-helpers dot-locking config network json-parser apple-hls-parser video uri->video)

;; These are not yet modularised
(include "download.scm")
(include "select-stream.scm")

;; This won't be modularised
(include "parse-flags.scm")

;; Tack "teve" onto the http user agent string
(client-software (cons (list program-display-name program-version #f) (client-software)))

(define (select-action default play? download? list? repl?)
  (debug* prepend: "Selected action:"
         (cond
          (repl? 'repl)
          ((and play? download? (not list?)) 'tee)
          ((and play? (not list?) (not download?)) 'play)
          ((and download? (not list?) (not play?)) 'download)
          ((and list? (not play?) (not download?)) 'list)
          ((not (or list? play? download?)) default)
          (else #f))))

(define (teve:repl uri id videos video)
  (set! *preferred-id* id)
  (set! *videos* videos)
  (set! *video* video)
  (set! *uri* uri)
  (stderr (conc program-display-name " " program-version))
  (stderr (conc " *uri*           " *uri*))
  (stderr (conc " *videos*        " (length *videos*)))
  (stderr (conc " *video*         " (length *video*)))
  (stderr (conc " *preferred-id*  " *preferred-id*))
  (stderr "READY.")
  (repl))

(define (teve:tee uri id video)
  (error "Teeing not yet implemented."))

(define (teve:list video)
  (let ((data (if (number? (*cfg* 'operators 'stream-id))
                  (stream-printer (video-ref (*cfg* 'operators 'stream-id) video))
                  (video-printer video))))
    (if (*cfg* 'operators 'machine-output)
        (stdout* data)
        (stderr* data))))

(define (teve:play uri id video)
  (if (and (number? id) (<= 0 id (length video)))
      (let ((play-command (stream->play-command (video-ref id video))))
        (if play-command
            (system (debug play-command))
            (stderr "I don't know how to play #" id)))
      (stderr (conc "Error: Could not find stream #" id "." #\newline
                    "Please verify that this stream-id exists for the specified uri, by " #\newline
                    "checking the output of '" (*platform* 'program-filename) " -l " uri "'"))))

(define (teve:download uri id video)
  (if (and (number? id) (<= 0 id (length video)))
      (let ((download-command (stream->download-command (video-ref id video)
                                                        (*cfg* 'operators 'output-filename))))
        (if download-command
            (system (debug download-command))
            (stderr "I don't know how to download #" id)))
      (stderr (conc "Error: Could not find stream #" id "." #\newline
                    "Please verify that this stream-id exists for the specified uri, by " #\newline
                    "checking the output of '" (*platform* 'program-filename) " -l " uri "'"))))

;;; Bootstrap
(receive (options operands)
  (args:parse (command-line-arguments) opts)
  (if (not (null? operands))
      (let* ((uri (car operands))
             (videos (uri->videos uri)))
        (if (null? videos)
            (stderr "No videos found.")
            ;; Until multiple videos are supported, video-id is always #f,
            ;; so we can just take the car instead of looping.
            (let* ((video (car videos))
                   (action (select-action
                            (*cfg* 'preferences 'default-action)
                            (*cfg* 'operators 'play?)
                            (*cfg* 'operators 'download?)
                            (*cfg* 'operators 'list?)
                            (*cfg* 'operators 'repl?)))
                   (id (if* (*cfg* 'operators 'stream-id)
                            it
                            (video->best-stream-id video))))
              (cond
               ((not id) (stderr (conc "Error: Video has no supported streams." #\newline
                                       "Please verify that all external programs are available.")))
               ((equal? action 'repl) (teve:repl uri id videos video))
               ((equal? action 'tee) (teve:tee uri id video))
               ((equal? action 'list) (teve:list video))
               ((equal? action 'play) (teve:play uri id video))
               ((equal? action 'download) (teve:download uri id video))
               (else (stderr "Pardon?"))))))))
