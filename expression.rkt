#lang racket

(require (lib "eopl.ss" "eopl"))
(require "errors.rkt")

(provide (all-defined-out))

;;; Expression
(define-datatype exp exp?
  (statements-exp
    (statements (list-of exp?)))
  (pass-exp)
  (break-exp)
  (continue-exp)
  (assignment-exp
    (ID string?)
    (rhs exp?))
  (return-stmt-exp
    (exp1 exp?))
  (none-exp)
  (global-stmt-exp
    (ID string?))
  (function-def-exp
    (ID string?)
    (params exp?)
    (statements exp?))
  (params-exp
    (params (list-of exp?)))
  (param-with-default-exp
    (ID string?)
    (rhs exp?))
  (if-stmt-exp
    (condition exp?)
    (true-statements exp?)
    (false-statements exp?))
  (for-stmt-exp
    (ID string?)
    (iterable exp?)
    (statements exp?))
  (or-exp
    (left exp?)
    (right exp?))
  (and-exp
    (left exp?)
    (right exp?))
  (not-exp
    (exp1 exp?))
  (eq-sum-exp
    (left exp?)
    (right exp?))
  (lt-sum-exp
    (left exp?)
    (right exp?))
  (gt-sum-exp
    (left exp?)
    (right exp?))
  (add-exp
    (left exp?)
    (right exp?))
  (sub-exp
    (left exp?)
    (right exp?))
  (mul-exp
    (left exp?)
    (right exp?))
  (div-exp
    (left exp?)
    (right exp?))
  (plus-factor-exp
    (power exp?))
  (minus-factor-exp
    (power exp?))
  (power-exp
    (atom exp?)
    (factor exp?))
  (list-element-exp
    (list exp?)
    (index exp?))
  (call-exp
    (function exp?)
    (arguments exp?))
  (arguments-exp
    (arguments (list-of exp?)))
  (var-exp
    (ID string?))
  (bool-exp
    (bool boolean?))
  (num-exp
    (num number?))
  (list-exp
    (list (list-of exp?)))
  (expressions-exp
    (expressions (list-of exp?))))


;;; Extractor functions
(define (exp->statements exp1)
  (cases exp exp1
    (statements-exp (statements) statements)
    (else report-type-mismatch 'statements-exp exp1)))

(define (exp->params exp1)
  (cases exp exp1
    (params-exp (params) params)
    (else report-type-mismatch 'params-exp exp1)))

(define (exp->arguments exp1)
  (cases exp exp1
    (arguments-exp (arguments) arguments)
    (else report-type-mismatch 'arguments-exp exp1)))

(define (exp->expressions exp1)
  (cases exp exp1
    (expressions-exp (expressions) expressions)
    (else report-type-mismatch 'expressions-exp exp1)))
