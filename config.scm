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

(use posix)

(require-extension miscmacros ini-file)

(include "misc-helpers.scm")

;;; Create a configuration variable
;;;
;;; Accepts zero or more configuration atrees and evaluates to a
;;; configuration lambda. New values are added and overridden from
;;; left to right, starting with the default configuration specified
;;; within this procedure.
;;;
;;; #f and unset values are considered equal.
;;;
;;; The resulting lambda may be used thus:
;;;   (cfg 'branch 'leaf)		=> #f	; value not yet set
;;;   (cfg set: 5 'branch 'leaf)	=> 5    ; set 'leaf to 5
;;;   (cfg 'branch 'leaf)		=> 5	; it's now set
(define make-configuration
  (lambda sources
    (let* ((default-configuration '((operators
                                     (default . download)
                                     (play? . #f)
                                     (download? . #f)
                                     (list? . #f)
                                     (stream-id . #f)
                                     (video-id . #f)
                                     (output-filename . #f))
                                    (external-programs
                                     (rtmpdump . "rtmpdump")
                                     (ffmpeg . "ffmpeg")
                                     (mplayer . "mplayer")
                                     (curl . "curl")
                                     (php . "php")
                                     (adobehds.php . "AdobeHDS.php"))
                                    (preferences
                                     (ideal-bitrate . 2500)
                                     (ideal-pixel-width . 1280))))
           (values (apply atree-fold-right
                     (cons default-configuration sources))))
      (lambda args
        (cond
         ((null? args) values)
         ((equal? set: (car args))
          (set! values (apply atree-update (cons values (cdr args))))
          (cadr args))
         (else
          (apply atree-ref values args)))))))

(define (env->conf env)
  (let* ((env-to-conf-map `(("TEVE_DEFAULT_OPERATION"
                             ,string->symbol operators default)
                            ("RTMPDUMP"
                             ,identity external-programs rtmpdump)
                            ("TEVE_RTMPDUMP"
                             ,identity external-programs rtmpdump)
                            ("FFMPEG"
                             ,identity external-programs ffmpeg)
                            ("TEVE_FFMPEG"
                             ,identity external-programs ffmpeg)
                            ("MPLAYER"
                             ,identity external-programs mplayer)
                            ("TEVE_MPLAYER"
                             ,identity external-programs mplayer)
                            ("CURL"
                             ,identity external-programs curl)
                            ("TEVE_CURL"
                             ,identity external-programs curl)
                            ("PHP"
                             ,identity external-programs php)
                            ("TEVE_PHP"
                             ,identity external-programs php)
                            ("TEVE_ADOBEHDS_PHP"
                             ,identity external-programs adobehds.php)
                            ("TEVE_BITRATE"
                             ,string->number preferences ideal-bitrate)
                            ("TEVE_WIDTH"
                             ,string->number preferences ideal-pixel-width)))
         (getenv (lambda (m)
                   (and-let* ((v (get-environment-variable (car m))))
                     ((cadr m) (get-environment-variable (car m))))))
         (conf-path cddr))
    (let loop ((mappings env-to-conf-map)
               (result '()))
      (cond
       ((null? mappings) result)
       ((getenv (car mappings))
        (loop (cdr mappings)
              (apply atree-update
                (cons result (cons (getenv (car mappings))
                                   (conf-path (car mappings)))))))
       (else
        (loop (cdr mappings) result))))))
    

(define *cfg*
  (let* ((sys-conf (if* (system-config-filename) (read-ini it) '()))
         (user-conf (if* (user-config-filename) (read-ini it) '()))
         (env-conf (env->conf (get-environment-variables))))
  (make-configuration sys-conf user-conf env-conf)))

