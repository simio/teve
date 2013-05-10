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

(use srfi-2)
(require-extension intarweb srfi-13 uri-common)

;;; Akamai is emotionally unstable and needs special treatment.
;;; The http servers might choke on url:s in GET requests containing
;;; url-encoded commas. This could be worked around in many ways, but
;;; better not mess with the low level stuff. Instead, just redefine the
;;; http-1.0-request-unparser to use a write-request-line which specifically
;;; decodes commas.
(define (hacks:url-decode-commas str)
  (string-translate* str '(("%2C" . ","))))

;;; This function UNMODIFIED from intarweb.scm
(define http-method->string symbol->string)

;;; This function is MODIFIED from intarweb.scm
(define (write-request-line/decode-commas request)
  (display (sprintf "~A ~A HTTP/~A.~A\r\n"
                    (http-method->string (request-method request))
                    (hacks:url-decode-commas		;; Only mod here.
                     (uri->string (request-uri request)))
                    (request-major request)
                    (request-minor request))
           (request-port request)))

;;; This function is MODIFIED from intarweb.scm
(define (http-1.0-request-unparser/decode-commas request)
  (and-let* (((= (request-major request) 1))
             ((= (request-minor request) 0))
             (o (request-port request)))
    (write-request-line/decode-commas request)
    (unparse-headers (request-headers request) o)
    (display "\r\n" o)
    request))

;;; Ordering matters, sez teh intarweb.
(request-unparsers (list (car (request-unparsers))
                         http-1.0-request-unparser/decode-commas))

;;; Finally, a request maker which guarantees niceness to fragile Akamai.
(define (make-emo-request url)
  (make-request method: 'GET
                uri: (absolute-uri url)
                major: 1
                minor: 0))
