#| Copyright (c) 2013 Jesper Raftegard <jesper@huggpunkt.org>
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

(use srfi-78)

(include "tests/global.scm")
(include "parsers/json.scm")

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
(check/expect-error (json-ref sanitised-json "video" -1))
