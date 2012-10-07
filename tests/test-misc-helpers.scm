(use utf8 srfi-78)

(include "misc-helpers.scm")

(define-syntax check/expect-error
  (syntax-rules ()
    ((check/except args ...)
     (check (handle-exceptions exn
                               'i-am-sorry-dave
                               args ...)
            => 'i-am-sorry-dave))))

(check-reset!)

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
    ("context" ("title" . "30/9 21:00")
     ("popoutUrl" . "/video/324252/30-9-21-00?type=embed"))))

;; json-read (from json egg) and sanitise-json-input
(check (with-input-from-string raw-json json-read) => unsanitised-json)
(check (sanitise-json-input unsanitised-json) => sanitised-json)
(check (sanitise-json-input #f) => #f)

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

(check-report)
