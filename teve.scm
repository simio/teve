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
(include "svt.scm")

(define (stream-printer stream)
  (let print-values ((rest stream)
                     (output ""))
    (if (null? rest)
        output
        (print-values (cdr rest)
                      (conc output
                            (string-pad-right (conc (caar rest) ":") 20)
                            (cdar rest) #\newline)))))

(define (video-printer video)
  (let print-streams ((rest video)
                      (output "")
                      (id 0))
    (if (null? rest)
        output
        (print-streams (cdr rest)
                       (conc output
                             (string-pad-right "id:" 20) id #\newline
                             (stream-printer (car rest)) #\newline)
                       (+ 1 id)))))

(define (video->download-command video outfile)
  (case (cdr (assv 'stream-type video))
    ((hds)
     (conc "php AdobeHDS.php"
           " --manifest \"" (cdr (assv 'url video)) "\""
           " --debug"
           " --outfile \"" outfile ".flv\""))
    ((hls)
     (conc "ffmpeg"
           " -i \"" (cdr (assv 'url video)) "\""
           " -acodec copy -vcodec copy -absf aac_adtstoasc"
           " \"" outfile ".avi\""))
    ((rtmp)
     (conc "rtmpdump"
           " -r \"" (cdr (assv 'url video)) "\""
           " -o \"" outfile ".flv\""
           " -W \"" (cdr (assv 'swf-player video)) "\""))
    ((http wmv)
     (conc "curl"
           " -Lo \"" outfile (if (eqv? 'http (cdr (assv 'stream-type video)))
                               ".flv"
                               ".wmv") "\""
           " \"" (cdr (assv 'url video)) "\""))
    (else
     #f)))

(define (url->video url)
  (svt:json-data->video (svt:download-json-data url)))

;;; Do something
(let ((command-line-args (parse-flags)))
  (if (not (null? command-line-args))
      (let* ((url (car command-line-args))
             (video (url->video url)))
        (case action
          ((list)
           (print* (video-printer video)))
          ((download)
           (if (and (< id (length video))
                    (not (negative? id)))
               (let ((download-command (video->download-command (list-ref video id) outfile)))
                 (if download-command
                     (print "$ " download-command)
                     (print "Unknown stream type: #" id)))
               (print "Could not find stream #" id "." #\newline
                      "Please verify #id with '" program-filename " -l " url "'")))
          (else
           (print "Pardon?"))))))
(exit 0)
