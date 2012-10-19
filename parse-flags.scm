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

(require-extension args)

(include "talk-is-cheap.scm")

(define opts
  (list (args:make-option (d) (required: "id") "Download stream #id"
                          (set! action 'download)
                          (set! stream-id (string->number arg))
                          (debug (conc "Downloading stream id " stream-id)))
        (args:make-option (v) #:none "Be verbose"
                          (set! talk-prints-debug-messages #t)
                          (debug "Verbose mode."))
        (args:make-option (o) (required: "filename")
                          "Filename to save to (default: movie)"
                          (set! outfile arg)
                          (debug (conc "Saving to \"" outfile "\"")))
        (args:make-option (p) (required: "id") "Play stream #id"
                          (set! action 'play)
                          (set! stream-id (string->number arg)))
        (args:make-option (h) #:none "Display this text"
                          (usage))
        (args:make-option (l) (optional: "id") "List stream(s)"
                          (set! action 'list)
                          (if arg (set! stream-id (string->number arg))))))

(define (usage)
  (with-output-to-port (current-error-port)
    (lambda ()
      (print "Usage: " program-filename " [options] <url>" #\newline
             (args:usage opts)
             "Report bugs to jesper at huggpunkt.org.")))
  (exit 1))
