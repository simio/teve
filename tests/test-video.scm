(use srfi-78 srfi-12)

(include "video.scm")

(check-reset!)

(define-syntax check/expect-error
  (syntax-rules ()
    ((check/except args ...)
     (check (handle-exceptions exn
                               'i-am-sorry-dave
                               args ...)
            => 'i-am-sorry-dave))))

;; Stream values
(check (make-stream-value 'key 5) => '(key . 5))
(check (make-stream-value 5 'value) => #f)
(check (stream-value? (make-stream-value 'key 5)) => #t)
(check (stream-value? (make-stream-value 5 'key)) => #f)
(check (stream-value? (list (list))) => #f)

;; Streams
(check (make-stream) => '())
(check (make-stream
        (make-stream-value 'url "http://example.com/")
        (make-stream-value "invalid" "value")
        (make-stream-value 'bitrate 192)
        (list '(sublist . isivanlid)))
       => '((url . "http://example.com/")(bitrate . 192)))

(define test-stream (make-stream
                     (make-stream-value 'url "http://example.com/")
                     (make-stream-value "invalid" "value")
                     (make-stream-value 'bitrate 192)))

(define test-stream-2 (make-stream
                       (make-stream-value 'url "http://example.net/")
                       "not-a-stream-value"
                       (make-stream-value 'bitrate 256)
                       (make-stream-value 'url "http://double.example.net/")))
(check test-stream-2 => (list (cons 'url "http://example.net/")
                              (cons 'bitrate 256)))

(check (stream? test-stream) => #t)
(check (stream? '(a . 4)) => #f)
(check (stream? 'symbol) => #f)
(check (stream? (list (make-stream-value 'url "http://example.com/") #f))
       => #f)

(check (stream-ref 'url test-stream) => "http://example.com/")
(check (stream-value? (assoc 'url test-stream)) => #t)
(check (stream-ref 'invalid test-stream) => #f)
(check (stream-ref 'url 'not-a-stream) => #f)

(check (update-stream (make-stream)) => '())
(check (update-stream test-stream
                      (make-stream-value 'bitrate 256))
       => '((bitrate . 256)
            (url . "http://example.com/")))
(check (update-stream test-stream
                      (make-stream-value 'referer "http://example.org/"))
       => '((referer . "http://example.org/")
            (url . "http://example.com/")
            (bitrate . 192)))
(check (update-stream test-stream
                      (make-stream-value 'referer "http://example.org/")
                      (make-stream-value 'bitrate 256))
       => '((referer . "http://example.org/")
            (bitrate . 256)
            (url . "http://example.com/")))

(check (pairs->stream (list
                       '(a . b)
                       '("fail" . 'here)
                       (assoc 'url test-stream)))
       => (make-stream
           (make-stream-value 'a 'b)
           (make-stream-value 'url "http://example.com/")))
(check/expect-error
 (expand '(pairs->stream)))
(check (pairs->stream '()) => '())

(check (stream-length test-stream)
       => 2)
(check/expect-error (stream-length 'not-a-list))
(check/expect-error (stream-length))
(check/expect-error (stream-length test-stream test-stream-2))

;; Videos
(check (make-video) => '())
(check (make-video test-stream) => (list test-stream))
(check (make-video #f) => '())
(check (make-video (list 'a 'b (list 'c) (cons 'a 5))) => '())
(check (make-video 'a 'b (list 'c) (list (cons 'a 5)) (list #f))
       => '(((a . 5))))

(check (video? (make-video)) => #t)
(check (video? 5) => #f)
(check (video? '()) => #t)
(check (video? '(())) => #t)
(check (video? '((()))) => #f)
(check (video? '(((a . 5)))) => #t)
(check (video? '(((5 . a)))) => #f)

(check (update-video (make-video test-stream))
       => (make-video test-stream))
(check (update-video (make-video test-stream-2) test-stream)
       => (list test-stream test-stream-2))
(check (update-video (make-video test-stream) test-stream)
       => (make-video test-stream))

(check/expect-error
 (expand '(streams->video)))
(check (streams->video (list test-stream test-stream-2))
       => (make-video test-stream test-stream-2))

(define test-video (make-video test-stream))
(define test-video-2 (make-video test-stream test-stream-2))

(check (video-ref 0 test-video)
       => '((url . "http://example.com/")
            (bitrate . 192)))
(check (stream? (video-ref 1 test-video-2)) => #t)
(check (video-ref 3 test-video-2) => #f)
(check (video-ref -1 test-video) => #f)
(check (video-ref 2 (list)) => #f)
(check (video-ref 0 'not-a-video) => #f)
(check/expect-error (video-ref 'not-a-number test-video))

(check (video-length test-video-2) => 2)
(check/expect-error (video-length 'not-a-list))
(check/expect-error (video-length))
(check/expect-error (video-length test-video test-video-2))




(check-report)
