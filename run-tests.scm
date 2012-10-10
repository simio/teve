(use srfi-13 srfi-78 posix)

(define-syntax check/expect-error
  (syntax-rules ()
    ((check/expect-errors args ...)
     (check (handle-exceptions exn
                               'i-am-sorry-dave
                               args ...)
            => 'i-am-sorry-dave))))

(check-reset!)

(let load-tests ((test-files (directory "tests")))
  (cond
   ((null? test-files)
    'all-tests-done)
   ((string-suffix? ".scm" (car test-files))
    (load (conc "tests/" (car test-files)))
    (load-tests (cdr test-files)))
   (else
    (load-tests (cdr test-files)))))

(check-report)
