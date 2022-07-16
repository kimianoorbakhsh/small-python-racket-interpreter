#lang racket

(provide report-type-mismatch)
(define (report-type-mismatch expected val)
  (printf "Type mismatched: Expected ~s but got ~s\n" expected val))
