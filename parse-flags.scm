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

(require-extension args)

(include "talk-is-cheap.scm")

(define opts
  (list (args:make-option (d) #:none
                          "Download stream (default or specified with -i)"
                          (*cfg* set: #t 'operators 'download?))
        (args:make-option (v) #:none "Be verbose"
                          (set! talk-prints-debug-messages #t)
                          (debug "Verbose mode."))
        (args:make-option (o) (required: "filename")
                          "Output filename"
                          (*cfg* set: arg 'operators 'output-filename)
                          (debug (conc
                                  "Saving to \""
                                  (*cfg* 'operators 'output-filename "\""))))
        (args:make-option (p) #:none
                          (conc "Play stream (default or specified with -i)")
                          (*cfg* set: #t 'operators 'play?))
        (args:make-option (l) #:none "List all streams (or specified with -i)"
                          (*cfg* set: #t 'operators 'list?))
        (args:make-option (i) (required: "id") "Stream #id to use"
                          (*cfg* set: (string->number arg)
                                 'operators 'stream-id))
        (args:make-option (h) #:none "Display this text" (usage))))

(define (usage)
  (with-output-to-port (current-error-port)
    (lambda ()
      (print "Usage: " (*platform* 'program-filename) " [options] <url>"
             #\newline (args:usage opts)
             (conc "Please report bugs to jesper at huggpunkt.org or"
                   " https://github.com/simio/teve"))))
  (exit 1))
