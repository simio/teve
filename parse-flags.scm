;;; Copyright (c) 2012 Jesper Raftegard <jeser@huggpunkt.org>
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

(require-extension srfi-37)

(define (parse-flags)
  (define flag-usage
    (option
     '(#\h) #f #f
     (lambda _
       (stderr (conc program-name " " program-version))
       (stderr (conc "Usage: " program-filename " [-hlv] [-d id] [-o outfile] url"))
       (stderr "  -d <id>   Download stream #<id>")
       (stderr "  -h        Show this message.")
       (stderr "  -l        List available streams.")
       (stderr "  -o <file> Download to <file>. (Default is 'movie'.)")
       (stderr "  -v        Be verbose.")
       (exit))))
  (define flag-download-id
    (option
     '(#\d) #t #f
     (lambda (option name arg seeds)
       (set! action 'download)
       (set! id (string->number arg))
       (debug (conc "Heading for stream id " id))
       seeds)))
  (define flag-verbose
    (option
     '(#\v) #f #f
     (lambda (option name arg seeds)
       (set! verbosity #t)
       (debug "Verbose mode.")
       seeds)))
  (define flag-outfile
    (option
     '(#\o) #t #f
     (lambda (option name arg seeds)
       (set! outfile arg)
       (debug (conc "Outfile set to '" outfile "'."))
       seeds)))
  (define flag-list-streams
    (option
     '(#\l) #f #f
     (lambda (option name arg seeds)
       (set! action 'list)
       seeds)))
  (reverse
   (args-fold
    (command-line-arguments)
    (list flag-usage flag-list-streams flag-download-id flag-outfile flag-verbose)
    (lambda (option name arg seeds)
      (stdout (conc "Unrecognized option " name)))
    cons
    '())))
