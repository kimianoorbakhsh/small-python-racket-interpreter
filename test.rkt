#lang racket

(require "rktpython.rkt")

(define tests-directory "tests/")
(define tests '(
  4_general
  5_general
  7_recursion
  12_lazy
  13_typing
  14_typing))

(let loop ([tests tests])
  (when (not (null? tests))
    (begin
      (printf "Running test: ~s\n" (car tests))
      (display "Actual:\n")
      (with-handlers ([exn:fail? (lambda (exn) (printf "Error: ~a\n" (exn-message exn)))])
        (evaluate (string-append tests-directory (symbol->string (car tests)) "_in.txt")))
      (display "Expected:\n")
      (display (file->string (string-append tests-directory (symbol->string (car tests)) "_out.txt")))
      (loop (cdr tests)))))
