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

(use srfi-13 srfi-14 srfi-78)

(include "tests/global.scm")
(include "misc-helpers.scm")

;; url->protocol
(check (url->protocol "http://www.example.com/") => "http")
(check (url->protocol "https://www.example.com/") => "https")
(check (url->protocol "hvhs:http://www.example.com/") => "hvhs:http")
(check (url->protocol "://tjo") => "")
(check (url->protocol "http") => #f)
(check (url->protocol 5) => #f)

;; add-http-get-query-var
(check (add-http-get-query-var "http://www.example.org/?var=val" "var1" "val1")
       => "http://www.example.org/?var=val&var1=val1")
(check (add-http-get-query-var "http://www.example.org" "var1" "val1")
       => "http://www.example.org?var1=val1")
(check (add-http-get-query-var "http://www.example.org/&fel=err" "var1" "val1")
       => "http://www.example.org/&fel=err?var1=val1")
(check (add-http-get-query-var
        "rtmp://server.exa?mple.org/stream" "var1" "val1")
       => "rtmp://server.exa?mple.org/stream&var1=val1")
(check (add-http-get-query-var "" 5 #f) => "?5=#f")
(check (add-http-get-query-var 'symbol 0 1) => #f)
(check (add-http-get-query-var "a" 'symbol '(list)) => "a?symbol=(list)")

;; varlist->alist
(define hls-style-varlist
  (conc "VAR1=hej,NUMBER=1024,BOOLEAN=#t,FALSE_WORD=false,FALSE_SYMBOL='false,"
        "VAR-WITHOUT-VALUE,1=2,symbol='en-symbol,false=#f,not-false='#f"))
(check (varlist->alist hls-style-varlist ",")
       => '(("not-false" . |#f|)
            ("false" . #f)
            ("symbol" . en-symbol)
            ("1" . 2)
            ("FALSE_SYMBOL" . false)
            ("FALSE_WORD" . "false")
            ("BOOLEAN" . #t)
            ("NUMBER" . 1024)
            ("VAR1" . "hej")))
(check (varlist->alist "" ",") => '())
(check (varlist->alist "nothing" "") => '())
(check (varlist->alist 'symbol 5) => #f)
(check (varlist->alist (list) (list)) => #f)

;; string-drop-to
(check (string-drop-to "meddelande: hejsan" ": ") => "hejsan")
(check (string-drop-to "teckenkontroll: ok" #\space) => "ok")
(check (string-drop-to "hela strängen tack" #\') => "hela strängen tack")
(check/expect-error (string-drop-to '() 5))
(check (string-drop-to "1234567" 3) => "4567")
(check (string-drop-to "abcde" "a") => "bcde")
(check (string-drop-to "abcde" "e") => "")
(check (string-drop-to "" 0) => "")
(check (string-drop-to "a be ce de e äff" 'de) => " e äff")

;; x-sep-resolution->pair
(check (x-sep-resolution->pair "1024x768") => '(1024 . 768))
(check (x-sep-resolution->pair "10X20") => '(10 . 20))
(check (x-sep-resolution->pair "") => #f)
(check/expect-error (x-sep-resolution->pair (list)))
(check/expect-error (x-sep-resolution->pair 'symbol))
(check (x-sep-resolution->pair "5xb") => #f)
(check (x-sep-resolution->pair "aX1") => #f)
(check (x-sep-resolution->pair "aXb") => #f)

;; make-rnd-string
(check (equal? (make-rnd-string 5) (make-rnd-string 5)) => #f)
(check (string-length (make-rnd-string 50)) => 50)
(check (string-delete char-set:letter (make-rnd-string 257)) => "")
(check (string-length (string-filter char-set:letter (make-rnd-string 1000)))
       => 1000)

(check (string-length (make-rnd-string 50 "7 chars")) => 57)
(check (string-delete char-set:letter (make-rnd-string 257 "&")) => "&")
(check (string-length (string-filter
                       char-set:letter
                       (make-rnd-string 1000 "")))
       => 1000)
(check (string-suffix? "()" (make-rnd-string 5 (list))) => #t)
(check (string-length (make-rnd-string 5 (list))) => 7)
