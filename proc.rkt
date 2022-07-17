#lang racket

(require (lib "eopl.ss" "eopl"))
(require racket/lazy-require)
(require "errors.rkt")
(lazy-require ["exp.rkt" (exp?)])

(provide (all-defined-out))

(define-datatype procedure proc?
  (a-proc
   (p-name string?)
   (params list?)
   (p-body exp?)))
