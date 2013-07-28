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

(require-extension miscmacros sha2 message-digest)

(include "platform")
(include "misc-helpers")
(include "dot-locking")

;;; Return a delayed download. If a second parameter is supplied,
;;; it is used as a reader. The default is read-string.
(define (network:delay-download url . rest)
  (let-optionals rest ((reader read-string))
    (delay
      (ignore-errors
       (with-input-from-request url #f reader)))))

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
(define (network:cache-object-alive? obj . rest)
  (let-optionals rest ((ttl #t))
    (let ((ttl (cond
                ((and (*cfg* 'preferences 'cache-override-ttl)
                      (*cfg* 'preferences 'cache-default-ttl)))
                ((not ttl) -1)
                ((number? ttl) ttl)
                (else (or (quick-ref obj 'ttl) -1)))))
      (< (current-seconds) (+ ttl (quick-ref obj 'timestamp))))))

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
    data))

;;; Store data in cache and return data.
(define (network:store key ttl data)
  (let ((object `((timestamp . ,(current-seconds))
                  (ttl . ,(if* ttl it (*cfg* 'preferences 'default-cache-ttl)))
                  (data . ,data)))
        (filename (network:key->filename key)))
    (if filename
        (with-dot-lock filename
          (with-output-to-file filename (lambda () (write object)))))
    (quick-ref object 'data)))

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
(define (network:cache-controller uri reader ttl)
  (let* ((download (network:delay-download uri))
         (key (network:uri->key (->string/uri uri)))
         (ttl (cond
               ((not ttl) -1)
               ((number? ttl) ttl)
               (else (*cfg* 'preferences 'cache-default-ttl))))
         (data (cond
                ((not (*cfg* 'preferences 'use-cache))
                 (force download))
                ((network:get-entry key))
                (else
                 ;; XXX: TTL should actually be derived from the
                 ;; relevant HTTP headers, if they exist...
                 (network:store key ttl (force download))))))
    (and (string? data)
         (with-input-from-string data reader))))

(define (download uri #!key (ttl #t) (reader read-string))
  (let ((value (network:cache-controller uri reader ttl)))
    (if value
        value
        (begin
          (stderr "The cache controller somehow failed.")
          #f))))
