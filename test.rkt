#lang racket

(require "rktpython.rkt")

(define tests-directory "tests/")
(define tests '(
  0_general
  1_general
  2_general
  3_general
  4_general
  5_general
  6_general
  7_recursion
  8_recursion
  9_lazy
  10_lazy
  11_lazy
  12_lazy
  13_typing
  14_typing
  15_typing))

(let loop ([tests tests])
  (when (not (null? tests))
    (begin
      (printf "Running test: ~s\n" (car tests))
      (display "Actual:\n")
      (with-handlers ([exn:fail? (lambda (exn) (printf "Error: ~a\n" (exn-message exn)))])
        (evaluate (string-append tests-directory (symbol->string (car tests)) "_in.txt")))
      (displayln "")
      (display "Expected:\n")
      (display (file->string (string-append tests-directory (symbol->string (car tests)) "_out.txt")))
      (displayln "")
      (loop (cdr tests)))))
