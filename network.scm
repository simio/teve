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

(use srfi-13 posix)

(require-extension miscmacros sha2 message-digest)

(include "platform")
(include "misc-helpers")
(include "dot-locking")

;;; Make a delayed download. If a second parameter is supplied,
;;; it is used as a reader. The default is read-string.
(define (network:delay-download uri #!optional (reader read-string))
  (delay
    (begin (debug (conc "Forcing delayed download of \"" uri "\""))
           (handle-exceptions exn
               (begin (stderr (conc "Failed to download \"" uri "\"" #\newline
                                    ((condition-property-accessor 'exn 'message) exn)))
                      (values #f #f #f #f))
             (let-values (((result request response) (with-input-from-request uri #f reader)))
               (values result request response #t))))))

;;; Tell whether a cache object is alive.
;;;
;;; Takes two arguments:
;;;     cached-object   A cache object.
;;;     ttl             The ttl the caller is asking for.
;;;                     (Optional. Default: #t)
;;;
;;; Evaluation:
;;;  1. If the cache-override-ttl config value is set, the config value
;;;     'cache-default-ttl is used.
;;;  2. Otherwise, if ttl is #f, the cache is always considered too old.
;;;  3. Otherwise, if ttl is a number, the cache will be considered too old
;;;     if it is older than that number of seconds.
;;;  4. Otherwise (for #t), the ttl stored in the cache is used.
;;;
;;; Returns #t iff the cached object is alive, otherwise #f.
(define (network:cache-object-alive? obj #!optional (ttl #t))
  (let ((ttl (cond
              ((and (*cfg* 'preferences 'cache-override-ttl)
                    (*cfg* 'preferences 'cache-default-ttl)))
              ((not ttl) -1)
              ((number? ttl) ttl)
              (else (or (quick-ref obj 'ttl) -1)))))
    (< (current-seconds) (+ ttl (quick-ref obj 'timestamp)))))

(define (network:uri->key uri)
  (message-digest-string (sha256-primitive) uri))

(define (network:key->filename key)
  (and-let* ((base-dir (*platform* 'cache-dir))
             (exists (directory-exists? base-dir)))
    (conc base-dir "/" key)))

;;; Fetch data for a certain key from cache iff there is any
;;; up-to-date, associated data in the cache. Return false if there is
;;; no data or it is too old.
;;;
;;; An optional second parameter may be specified. It must be a
;;; number. This number will be used instead of the cached
;;; ttl value, to determine whether the cached data is considered
;;; up to date or not.
(define (network:get-entry key)
  (and-let* ((filename (network:key->filename key))
             (in-cache (file-exists? filename))
             (cache-object (with-input-from-file filename read))
             (up-to-date (network:cache-object-alive? cache-object))
             (data (quick-ref cache-object 'data)))
    (debug (conc "Reading from cache: " (quick-ref cache-object 'uri)))
    data))

;;; Store data in cache and return data.
(define (network:store uri key ttl data)
  (let ((object `((uri . ,uri)
                  (timestamp . ,(current-seconds))
                  (ttl . ,(if* ttl it (*cfg* 'preferences 'default-cache-ttl)))
                  (data . ,data)))
        (filename (network:key->filename key)))
    (if filename
        (with-dot-lock filename
          (debug (conc "Attempting to cache " uri " in " filename))
          (with-output-to-file filename (lambda () (write object)))))
    (quick-ref object 'data)))

;;; Create a TTL in seconds, given a HTTP/1.1 Cache-control max-age
;;; value, a HTTP/1.0 Expires value and a fallback (in seconds).
(define (network:select-cache-ttl max-age expires fallback)
  (cond ((number? max-age) max-age)
        ((number? expires) expires)
        ((vector? expires) (- (utc-time->seconds expires) (current-seconds)))
        (else fallback)))

;;; Translate requests to data from cache or network.
;;;  1. If caching is disabled, download.
;;;  2. Otherwise, if the uri is cached and up-to-date, use cached data.
;;;  3. Otherwise, download, cache and return data.
;;;
;;; Valid values for the uri parameter are uri objects, http-client
;;; requests or a simple uri in a string. (The value will be passed
;;; directly to with-input-from-request from the http-client egg.)
;;;
;;; ttl is the number of seconds a cached object is considered up to data
;;; to use when storing it, which is later used for subsequent checks if
;;; the object is up-to-date.
(define (network:cache-controller uri reader ttl-param)
  (and-let* ((download (network:delay-download uri))
             (uri-string (->string/uri uri))
             (key (network:uri->key uri-string))
             (fallback-ttl (cond
                            ((not ttl-param) -1)
                            ((number? ttl-param) ttl-param)
                   (else (*cfg* 'preferences 'cache-default-ttl))))
             (data (cond
                    ((not (*cfg* 'preferences 'use-cache))
                     (debug (conc "Cache disabled. Fetching " uri-string))
                     (force download))
                    ((network:get-entry key))
                    (else
                     (let-values (((result request-uri response success) (force download)))
                       (and success
                            (let* ((max-age (cdip (header-value 'cache-control
                                                                (response-headers response))))
                                   (expires (header-value 'expires (response-headers response)))
                                   ;; The Cache-control max-age value should override the Expires
                                   ;; header value, if both are present. If none are present,
                                   ;; use the default ttl which the caller supplied.
                                   (ttl (network:select-cache-ttl max-age expires fallback-ttl)))
                              (debug (conc "Storing in cache; ttl "
                                           ttl " (" max-age "/" expires ")"))
                              (network:store uri-string key ttl result))))))))
    (and (string? data)
         (with-input-from-string data reader))))

(define (fetch u #!key (ttl #t) (reader read-string))
  (let* ((uri (if (and (string? u) (string-contains (uri->base-path u) "akamaihd.net"))
                  (make-emo-request u)
                  u)))
    (network:cache-controller uri reader ttl)))
