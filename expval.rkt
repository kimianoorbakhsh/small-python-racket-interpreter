#lang racket

(require (lib "eopl.ss" "eopl"))
(require "proc.rkt")
(require "errors.rkt")

(provide (all-defined-out))

(define-datatype expval expval?
  (num-val (num number?))
  (bool-val (bool boolean?))
  (list-val (lst list?))
  (none-val)
  (proc-val (proc proc?))
  (void-val)
  (ref-val (ref number?))
  (break-val)
  (continue-val)
  (return-val (val expval?)))

(define (expval->num val)
  (cases expval val
    (num-val (num) num)
    (else (report-type-mismatch 'num val))))

(define (expval->bool val)
  (cases expval val
    (bool-val (bool) bool)
    (else (report-type-mismatch 'bool val))))

(define (expval->list val)
  (cases expval val
    (list-val (lst) lst)
    (else (report-type-mismatch 'list val))))

(define (expval->ref val)
  (cases expval val
    (ref-val (ref) ref)
    (else (report-type-mismatch 'ref val))))

(define (expval->proc val)
  (cases expval val
    (proc-val (proc) proc)
    (else (report-type-mismatch 'proc val))))
