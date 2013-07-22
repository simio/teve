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

(define (cache:uri->key uri)
  (message-digest-string (sha256-primitive) uri))

(define (cache:key->filename key)
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
(define (cache:get-entry key . rest)
  (and-let* ((filename (cache:key->filename key))
             (in-cache (file-exists? filename))
             (cache-object (with-input-from-file filename read))
             (ttl (or (and (not (null? rest))
                           (number? (car rest))
                           (car rest))
                      (and (*cfg* 'preferences 'cache-override-ttl)
                           (*cfg* 'preferences 'cache-default-ttl))
                      (quick-ref cache-object 'ttl)))
             (timestamp (quick-ref cache-object 'timestamp))
             (up-to-date (< (current-seconds) (+ timestamp ttl)))
             (data (quick-ref cache-object 'data)))
    data))

;;; Store data in cache and return data.
(define (cache:store key ttl data)
  (let ((object `((timestamp . ,(current-seconds))
                  (ttl . ,(if* ttl it (*cfg* 'preferences 'default-cache-ttl)))
                  (data . ,data)))
        (filename (cache:key->filename key)))
    (if filename
        (with-dot-lock filename
          (with-output-to-file filename (lambda () (write object)))))
    object))

;;; Wrap it all up. Possible actions:
;;;  1. If caching is disabled, download.
;;;  2. Otherwise, if the uri isn't cached, download and update cache.
;;;  3. Otherwise, if the cache is outdated, download and update cache.
;;;  4. Otherwise, just use the cached data.
;;;
;;; Any second parameter supplied must be a number and specifies the
;;; ttl to use for checking if the cache is up to date. If no number
;;; is specified, the ttl value used to store the data in cache is used.
(define (via-cache uri . rest)
  (let* ((data (delay-download uri))
         (key (cache:uri->key uri))
         (ttl (or (and (not (null? rest))
                       (number? (car rest))
                       (car rest))
                  #f)))
    (cond
     ((not (*cfg* 'preferences 'use-cache))
      (force data))
     ((cache:get-entry key ttl))
     (else
      ;;; XXX: TTL should actually be derived from the relevant HTTP headers,
      ;;; if they exist...
      (cache:store key ttl (force data))))))