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
(define program-version "0.2.2")

;; Keep this here to avoid reimporting uri-common stuff
(include "intarweb-hack.scm")   ; loads uri-common; don't load it elsewhere

(include "platform.scm")

(include "stdinerr")
(import stdinerr)

(include "misc-helpers.scm")

(include "dot-locking.scm")
(include "config.scm")
(include "network.scm")

(include "parse-flags.scm")     ; Should load after config.scm

(include "video.scm")
(include "url2vid.scm")
(include "select-stream.scm")
(include "download.scm")

(define (select-action default play? download? list? repl?)
  (debug* "Performing ")
  (debug
   (cond
    (repl? 'repl)
    ((and play? download? (not list?)) 'tee)
    ((and play? (not list?) (not download?)) 'play)
    ((and download? (not list?) (not play?)) 'download)
    ((and list? (not play?) (not download?)) 'list)
    ((not (or list? play? download?)) default)
    (else #f))))

(define (teve:repl uri id videos video)
  (let ((*preferred-id* id)
        (*videos* videos)
        (*video* video)
        (*uri* uri))
    (stderr program-display-name " " program-version)
    (stderr " *uri*           " *uri*)
    (stderr " *videos*        " (length *videos*))
    (stderr " *video*         " (length *video*))
    (stderr " *preferred-id*  " *preferred-id*)
    (stderr "REPL READY.")
    (repl)))

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
      (stderr "Error: Could not find stream #" id "." #\newline
              "Please verify that this stream-id exists for the specified uri, by " #\newline
              "checking the output of '" (*platform* 'program-filename) " -l " uri "'")))

(define (teve:download uri id video)
  (if (and (number? id) (<= 0 id (length video)))
      (let ((download-command (stream->download-command (video-ref id video)
                                                        (*cfg* 'operators 'output-filename))))
        (if download-command
            (system (debug download-command))
            (stderr "I don't know how to download #" id)))
      (stderr "Error: Could not find stream #" id "." #\newline
              "Please verify that this stream-id exists for the specified uri, by " #\newline
              "checking the output of '" (*platform* 'program-filename) " -l " uri "'")))

;;; Bootstrap
(receive (options operands)
  (args:parse (command-line-arguments) opts)
  (if (not (null? operands))
      (let* ((uri (car operands))
             (videos (url->videos uri)))
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
               ((equal? action 'repl) (teve:repl uri id videos video))
               ((equal? action 'tee) (teve:tee uri id video))
               ((equal? action 'list) (teve:list video))
               ((equal? action 'play) (teve:play uri id video))
               ((equal? action 'download) (teve:download uri id video))
               (else (stderr "Pardon?"))))))))
