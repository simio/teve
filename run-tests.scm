(use posix)

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
