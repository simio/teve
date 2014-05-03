;;; Copyright (c) 2013 Jesper Raftegard <jesper@huggpunkt.org>
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

(use srfi-1 srfi-13 srfi-18 srfi-69 data-structures extras files ports posix utils 
     http-client ini-file intarweb json message-digest miscmacros ssax sha2 uri-common)

;; Ordering matters here
(include "http-client.scm")   ; reexports uri-common, intarweb and http-client
(include "scheme-prelude/stdouterr.scm")
(include "scheme-prelude/prelude.scm")
(include "platform.scm")
(include "misc-helpers.scm")
(include "dot-locking.scm")
(include "config.scm")
(include "network.scm")

;; This ordering is particularly ugly
(include "parsers/json.scm")
(include "video.scm")
(include "parsers/apple-hls.scm")

;; This should be automated
(include "sites/svt.scm")
(include "sites/tv4.scm")
(include "sites/youtube-dl.scm")

;; This should be renamed
(include "uri2vid.scm")

(include "download-commands.scm")
(include "stream-selection.scm")
