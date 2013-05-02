#| Copyright (c) 2012, 2013 Jesper Raftegard <jesper@huggpunkt.org>
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

(define program-name "teve")
(define program-filename program-name)
(define program-version "0.1")

(define action 'nothing)
(define stream-id #f)
(define video-id 1)		; No multiple video support just yet
(define outfile #f)

(include "talk-is-cheap.scm")
(include "misc-helpers.scm")
(include "parse-flags.scm")
(include "video.scm")
(include "url2vid.scm")
(include "select-stream.scm")
(include "download.scm")

;;; Do something
(receive (options operands)
  (args:parse (command-line-arguments) opts)
  (if (not (null? operands))
      (let* ((url (car operands))
             (videos (url->videos url)))
        (if (null? videos)
            (stderr "No videos found.")
            ;; Until multiple videos are supported, video-id is always 1,
            ;; so we can just take the car instead of looping.
            (let* ((video (car videos))
                   (id (if* stream-id it (video->best-stream-id video))))
              (case action
                ((list)
                 ;; The list action is supposed to use 'stream-id rather than
                 ;; 'id, since it is acting directly on CLI parameters, rather
                 ;; than the automatically preferred or user specified stream.
                 (if (number? stream-id)
                     (stderr* (stream-printer (video-ref stream-id video)))
                     (stderr* (video-printer video))))
                ((play)
                 (if (and (number? id)
                          (<= 0 id (length video)))
                     (let ((play-command
                            (stream->play-command (video-ref id video))))
                       (if play-command
                           (stdout play-command)
                           (stderr "I don't know how to play #" id)))
                     (stderr "Error: Could not find stream #" id
                             "." #\newline
                             "Please verify that this stream-id exists for the "
                             "specified url, by checking the" #\newline
                             "output of '" program-filename " -l " url "'")))
                ((download)
                 (if (and (number? id)
                          (<= 0 id (length video)))
                     (let ((download-command
                            (stream->download-command (video-ref id video)
                                                      outfile)))
                       (if download-command
                           (stdout download-command)
                           (stderr "I don't know how to download #" id)))
                     (stderr "Error: Could not find stream #" id
                             "." #\newline
                             "Please verify that this stream-id exists for the "
                             "specified url, by checking the" #\newline
                             "output of '" program-filename " -l " url "'")))
                (else
                 (stderr "Pardon?"))))))))
