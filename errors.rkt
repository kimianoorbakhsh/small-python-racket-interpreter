#lang racket

(require (lib "eopl.ss" "eopl"))
(provide (all-defined-out))

(define (report-type-mismatch expected val)
  (printf "Type mismatched: Expected ~s but got ~s\n" expected val))

(define (report-no-binding-found search-var)
  (eopl:error 'apply-env "No binding for ~s" search-var))

(define (report-invalid-env env)
  (eopl:error 'apply-env "Bad environment: ~s" env))

(define (report-invalid-reference ref)
  (eopl:error 'setref! "Invalid refrence: ~s" ref))

(define (report-must-not-reach-here)
  (println "Must not reach here."))

(define (report-arguments-len-long)
  (println "Arguments length is too long."))
