
#lang racket

(define foo%
  (class* object% (printable<%>)
    (init)
    (super-new)

    (define/public (custom-print port qqdepth)
      (write-string "foo%/custom-print" port))

    (define/public (custom-write port)
      (write-string "foo%/custom-write" port))

    (define/public (custom-display port)
      (write-string "foo%/custom-display" port))))

(define foo (new foo%))
foo
;; (print foo)
;; (display foo)
;; (write foo)
