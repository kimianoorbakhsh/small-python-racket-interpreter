#lang racket

(require (lib "eopl.ss" "eopl"))
(require "errors.rkt")

(provide (all-defined-out))

(define-datatype procedure proc?
  (a-proc
   (p-name string?)
   (params list?)
   (p-body exp?)))
