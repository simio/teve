(define-syntax check/expect-error
  (syntax-rules ()
    ((check/expect-errors args ...)
     (check (handle-exceptions exn
                               'i-am-sorry-dave
                               args ...)
            => 'i-am-sorry-dave))))
