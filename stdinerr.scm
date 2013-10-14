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

(module stdinerr *
(import scheme chicken data-structures srfi-1 extras)

(define print-debug-messages?
  (let ((print-them #f))
    (lambda args
      (if (< 0 (length args))
          (begin
            (set! print-them (car args))
            (if (car args)
                (display (conc "Verbose mode enabled." #\newline)
                         (current-error-port))
                (display (conc "Verbose mode disabled." #\newline)
                         (current-error-port)))))
      print-them)))

(define current-debug-port (current-error-port))

;;; Print stuff. If stuff consists entirely of strings, numbers and chars,
;;; they are conced and displayed instead of pretty-printed. The return
;;; value is the last member of stuff (like with (begin)). Calling
;;; talk-is-cheap with less than 6 arguments is an error.
(define (talk-is-cheap port prepend append wrapper-mode mapper . stuff)
  (let* ((data (delay stuff))
         (print-data (if mapper
                         (map mapper (force data))
                         (force data))))
    (display prepend port)
    (if (every (lambda (obj) (or (string? obj) (number? obj) (char? obj)))
               (force data))
        (display (apply conc print-data) port)
        (pretty-print print-data port))
    (if wrapper-mode
        (begin
          (display append port)
          (last (force data)))
        (display append port))))

(define-syntax print-to-port*
  (syntax-rules ()
    ((print-to-port* port stuff ...)
     (talk-is-cheap port "" "" #f #f stuff ...))))

(define-syntax print-to-port
  (syntax-rules ()
    ((print-to-port port stuff ...)
     (talk-is-cheap port "" #\newline #f #f stuff ...))))

(define-syntax debug-is-cheap
  (syntax-rules (nl: prepend: mapper:)
    ((debug-is-cheap nl: newline prepend: prepend mapper: mapper stuff ...)
     (if (print-debug-messages?)
         (talk-is-cheap current-debug-port
                        (conc ";;; " prepend (if (< 0 (string-length prepend))
                                                 ": "
                                                 ""))
                        (if newline #\newline "")	; append
                        #t				; wrapper-mode
                        mapper
                        stuff ...)
         (begin stuff ...)))
    ((debug-is-cheap nl: newline prepend: prepend stuff ...)
     (debug-is-cheap nl: newline prepend: prepend mapper: #f stuff ...))
    ((debug-is-cheap nl: newline mapper: mapper prepend: prepend stuff ...)
     (debug-is-cheap nl: newline prepend: prepend mapper: mapper stuff ...))
    ((debug-is-cheap nl: newline mapper: mapper stuff ...)
     (debug-is-cheap nl: newline prepend: "" mapper: mapper stuff ...))
    ((debug-is-cheap nl: newline stuff ...)
     (debug-is-cheap nl: newline prepend: "" stuff ...))))

(define-syntax stdout
  (syntax-rules ()
    ((stdout stuff ...)
     (print-to-port (current-output-port) stuff ...))))

(define-syntax stdout*
  (syntax-rules ()
    ((stdout* stuff ...)
     (print-to-port* (current-output-port) stuff ...))))

(define-syntax stderr
  (syntax-rules ()
    ((stderr stuff ...)
     (print-to-port (current-error-port) stuff ...))))

(define-syntax stderr*
  (syntax-rules ()
    ((stderr* stuff ...)
     (print-to-port* (current-error-port) stuff ...))))

(define-syntax debug
  (syntax-rules ()
    ((debug stuff ...)
     (debug-is-cheap nl: #t stuff ...))))

(define-syntax debug*
  (syntax-rules ()
    ((debug* stuff ...)
     (debug-is-cheap nl: #f stuff ...))))

)
