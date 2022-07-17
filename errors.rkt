#lang racket

(require (lib "eopl.ss" "eopl"))
(provide (all-defined-out))

(define (report-type-error function)
  (eopl:error function "Type error"))

(define (report-type-mismatch function expected val)
  (eopl:error function "Type mismatched: Expected ~s but got ~s" expected val))

(define (report-no-binding-found search-var)
  (eopl:error 'apply-env "No binding for ~s" search-var))

(define (report-invalid-env env)
  (eopl:error 'apply-env "Bad environment: ~s" env))

(define (report-invalid-reference ref)
  (eopl:error 'setref! "Invalid refrence: ~s" ref))

(define (report-must-not-reach-here)
  (eopl:error 'value-of "Must not reach here."))

(define (report-arguments-len-long)
  (eopl:error 'value-of "Arguments length is too long."))

(define (report-file-does-not-exist file-name)
  (eopl:error 'evaluate "File ~s does not exist.\n" file-name))
