(use srfi-13 srfi-14 srfi-78)

(include "tests/global.scm")
(include "misc-helpers.scm")

;;; I have no idea why, but if these long strings are not broken up
;;; into pieces like this, csi as an inferior scheme in Emacs will
;;; print a lot of ^G instead of whatever.

(define raw-json
  (conc "{\"videoId\":324252,\"video\":{\"videoReferences\":[{\"url\":"
        "\"http://svtplay3d-f.akamaihd.net/z/world/open/20120930/1308924"
        "-006A/AKTUELLT_SO_HT-006A-c2abf0c0da84d8ef_,900,320,420,620,.mp4"
        ".csmil/manifest.f4m\",\"bitrate\":0,\"playerType\":\"flash\"},{"
        "\"url\":\"http://svtplay3d-f.akamaihd.net/i/world/open/20120930/"
        "1308924-006A/AKTUELLT_SO_HT-006A-c2abf0c0da84d8ef_,900,320,420,"
        "620,.mp4.csmil/master.m3u8\",\"bitrate\":0,\"playerType\":\"ios\"}]"
        ",\"subtitleReferences\":[{\"url\":\"\"}],\"position\":0,\"material"
        "Length\":843,\"live\":false,\"availableOnMobile\":true},\"statistics"
        "\":{\"client\":\"svt-play\",\"mmsClientNr\":\"1001001\",\"context\":"
        "\"svt-play\",\"programId\":\"1308924-06\",\"mmsCategory\":\"1\","
        "\"broadcastDate\":\"20120930\",\"broadcastTime\":\"2100\",\"category"
        "\":\"nyheter\",\"statisticsUrl\":\"http://ld.svt.se/svt/svt/s?svt"
        "-play.nyheter.aktuellt.hela-program.30-9-21-00\",\"title\":\"30-9-"
        "21-00\",\"folderStructure\":\"aktuellt.hela-program\"},\"context"
        "\":{\"title\":\"30/9 21:00\",\"popoutUrl\":\"/video/324252/30-9"
        "-21-00?type=embed\"}}"))

(define unsanitised-json
  `#(("videoId" . 324252)
     ("video" . #(("videoReferences"
                   #(("url" . ,(conc "http://svtplay3d-f.akamaihd.net/z/"
                                     "world/open/20120930/1308924-006A/"
                                     "AKTUELLT_SO_HT-006A-c2abf0c0da84d8"
                                     "ef_,900,320,420,620,.mp4.csmil/"
                                     "manifest.f4m"))
                     ("bitrate" . 0)
                     ("playerType" . "flash"))
                   #(("url" . ,(conc "http://svtplay3d-f.akamaihd.net/i/"
                                     "world/open/20120930/1308924-006A/"
                                     "AKTUELLT_SO_HT-006A-c2abf0c0da84d8ef_"
                                     ",900,320,420,620,.mp4.csmil/master.m3u8"))
                     ("bitrate" . 0)
                     ("playerType" . "ios")))
                  ("subtitleReferences"
                   #(("url" . ""))) ("position" . 0)
                   ("materialLength" . 843)
                   ("live" . #f)
                   ("availableOnMobile" . #t)))
     ("statistics" . #(("client" . "svt-play")
                       ("mmsClientNr" . "1001001")
                       ("context" . "svt-play")
                       ("programId" . "1308924-06")
                       ("mmsCategory" . "1")
                       ("broadcastDate" . "20120930")
                       ("broadcastTime" . "2100")
                       ("category" . "nyheter")
                       ("statisticsUrl" . ,(conc "http://ld.svt.se/svt/svt/"
                                                 "s?svt-play.nyheter.aktuellt"
                                                 ".hela-program.30-9-21-00"))
                       ("title" . "30-9-21-00")
                       ("folderStructure" . "aktuellt.hela-program")))
     ("context" . #(("title" . "30/9 21:00")
                    ("popoutUrl" . "/video/324252/30-9-21-00?type=embed")))))

(define sanitised-json
  `(("videoId" . 324252)
    ("video" ("videoReferences"
              (("url" . ,(conc "http://svtplay3d-f.akamaihd.net/z/world/open"
                               "/20120930/1308924-006A/AKTUELLT_SO_HT-006A-"
                               "c2abf0c0da84d8ef_,900,320,420,620,.mp4.csmil"
                               "/manifest.f4m"))
               ("bitrate" . 0)
               ("playerType" . "flash"))
              (("url" . ,(conc "http://svtplay3d-f.akamaihd.net/i/world/"
                               "open/20120930/1308924-006A/AKTUELLT_SO_HT-"
                               "006A-c2abf0c0da84d8ef_,900,320,420,620,"
                               ".mp4.csmil/master.m3u8"))
               ("bitrate" . 0)
               ("playerType" . "ios")))
     ("subtitleReferences" (("url" . "")))
     ("position" . 0)
     ("materialLength" . 843)
     ("live" . #f)
     ("availableOnMobile" . #t))
    ("statistics" ("client" . "svt-play")
     ("mmsClientNr" . "1001001")
     ("context" . "svt-play")
     ("programId" . "1308924-06")
     ("mmsCategory" . "1")
     ("broadcastDate" . "20120930")
     ("broadcastTime" . "2100")
     ("category" . "nyheter")
     ("statisticsUrl" . ,(conc "http://ld.svt.se/svt/svt/s?svt-play.nyheter"
                               ".aktuellt.hela-program.30-9-21-00"))
     ("title" . "30-9-21-00")
     ("folderStructure" . "aktuellt.hela-program"))
    ("context"
     ("title" . "30/9 21:00")
     ("popoutUrl" . "/video/324252/30-9-21-00?type=embed"))))

;; json-read (from json egg), sanitise-json-input and json-read-and-sanitise
(check (with-input-from-string raw-json json-read) => unsanitised-json)
(check (sanitise-json-input unsanitised-json) => sanitised-json)
(check (sanitise-json-input #f) => #f)
(check (with-input-from-string raw-json json-read-and-sanitise) => sanitised-json)
(check (with-input-from-string "blabla" json-read-and-sanitise) => #f)

;; json-ref (and quick-ref, since they're the same lambda)
(check (json-ref sanitised-json "context" "title") => "30/9 21:00")
(check (json-ref sanitised-json "finns inte") => #f)
(check (json-ref 5 0) => #f)
(check (json-ref (list) (list)) => #f)
(check (json-ref sanitised-json "video" "videoReferences" "url") => #f)
(check (json-ref sanitised-json "video" "videoReferences" 0 "playerType") => "flash")
(check (json-ref sanitised-json "video" "videoReferences" 0 'playerType) => #f)
(check (json-ref sanitised-json "video" "videoReferences" 1 "bitrate") => 0)
(check (json-ref sanitised-json "video" "videoReferences" 0 "playerType" "flash") => #f)
(check (json-ref sanitised-json) => sanitised-json)

(define test-sxml-sublist '((three items here) ("two" items) (1)))
(define test-sxml `(5 symbol ,test-sxml-sublist))

;; sxml-ref (test mostly differences from json-ref)
(check (sxml-ref test-sxml) => test-sxml)
(check (sxml-ref test-sxml 'symbol) => #f)
(check (sxml-ref test-sxml 1) => 'symbol)
(check (sxml-ref test-sxml "two") => #f)
(check (sxml-ref test-sxml 2) => test-sxml-sublist)
(check (sxml-ref test-sxml 2 "two") => 'items)
(check (sxml-ref test-sxml 2 1) => '("two" items))
(check (sxml-ref test-sxml 2 'three) => '(items here))
(check (sxml-ref test-sxml 2 'three 1) => 'here)
(check (sxml-ref test-sxml 2 'three 2) => #f)

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

;; cdip
(check (cdip '(a . b)) => 'b)
(check (cdip '(a)) => '())
(check (cdip 'symbol) => #f)
(check (cdip (list)) => #f)

;; caip
(check (caip '(a . b)) => 'a)
(check (caip '(a)) => 'a)
(check (caip 'symbol) => #f)
(check (caip (list)) => #f)

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

;; shell-escape
(define shell-special-chars "&|$\"'\\()[]{}<>#~=;,*")
(define shell-normal-chars (conc "abcdefghijklmnopqrstuvwxyzåäö"
                                 "ABCDEFGHIJKLMNOPQRSTUVWXYZÅÄÖ"
                                 "0123456789_-.:!%/"))
(check (shell-escape shell-special-chars)
       => "\\&\\|\\$\\\"\\'\\\\\\(\\)\\[\\]\\{\\}\\<\\>\\#\\~\\=\\;\\,\\*")
(check (shell-escape shell-normal-chars) => shell-normal-chars)
(check/expect-error (shell-escape 'symbol))
(check/expect-error (shell-escape (list)))

;; string-replace-every
(check (string-replace-every "needle" "bear" "there's a needle in my box")
       => "there's a bear in my box")
(check (string-replace-every "x" "/" "123x123") => "123/123")
(check (string-replace-every "" "" "") => "")
(check (string-replace-every "" "/" "xxx") => "xxx")
(check (string-replace-every "x" (list) "x") => "()")
(check/expect-error (string-replace-every 1 "" 3))
(check/expect-error (string-replace-every "" "" 1))
(check/expect-error (string-replace-every 1 "" ""))

;; first-html-attribute
