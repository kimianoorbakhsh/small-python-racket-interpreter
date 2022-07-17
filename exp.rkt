#lang racket

(require (lib "eopl.ss" "eopl"))
(require "errors.rkt")
(require "env.rkt")
(require "store.rkt")
(require "expval.rkt")

(provide (all-defined-out))

;;; Expression
(define-datatype exp exp?
  (statements-exp
    (statements (list-of exp?)))
  (pass-exp)
  (break-exp)
  (continue-exp)
  (assignment-exp
    (lhs exp?)
    (rhs exp?))
  (assignment-lhs-exp
    (ID string?)
    (type string?))
  (type-exp
    (type string?))
  (return-stmt-exp
    (exp1 exp?))
  (none-exp)
  (global-stmt-exp
    (ID string?))
  (print-stmt-exp
    (value exp?))
  (function-def-exp
    (ID string?)
    (params exp?)
    (return-type string?)
    (statements exp?))
  (params-exp
    (params (list-of exp?)))
  (param-with-default-exp
    (lhs exp?)
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
    (expressions (list-of exp?)))
  (list-const-exp
    (values list?))
  (thunk-exp
    (the-thunk thunk?)))


;;; Extractor functions
(define (exp->statements exp1)
  (cases exp exp1
    (statements-exp (statements) statements)
    (else (report-type-mismatch 'exp->statements 'statements-exp exp1))))

(define (exp->params exp1)
  (cases exp exp1
    (params-exp (params) params)
    (else (report-type-mismatch 'exp->params 'params-exp exp1))))

(define (exp->arguments exp1)
  (cases exp exp1
    (arguments-exp (arguments) arguments)
    (else (report-type-mismatch 'exp->arguments 'arguments-exp exp1))))

(define (exp->expressions exp1)
  (cases exp exp1
    (expressions-exp (expressions) expressions)
    (else (report-type-mismatch 'exp->expressions 'expressions-exp exp1))))

(define (exp->type exp1)
  (cases exp exp1
    (type-exp (type) type)
    (else (report-type-mismatch 'exp->type 'type-exp exp1))))

(define (replace-var-exps exp1)
  (cases exp exp1
    (none-exp ()
      exp1)
    (or-exp (left right)
      (or-exp
        (replace-var-exps left)
        (replace-var-exps right)))
    (and-exp (left right)
      (and-exp
        (replace-var-exps left)
        (replace-var-exps right)))
    (not-exp (exp1)
      (not-exp
        (replace-var-exps exp1)))
    (eq-sum-exp (left right)
      (eq-sum-exp
        (replace-var-exps left)
        (replace-var-exps right)))
    (lt-sum-exp (left right)
      (lt-sum-exp
        (replace-var-exps left)
        (replace-var-exps right)))
    (gt-sum-exp (left right)
      (gt-sum-exp
        (replace-var-exps left)
        (replace-var-exps right)))
    (add-exp (left right)
      (add-exp
        (replace-var-exps left)
        (replace-var-exps right)))
    (sub-exp (left right)
      (sub-exp
        (replace-var-exps left)
        (replace-var-exps right)))
    (mul-exp (left right)
      (mul-exp
        (replace-var-exps left)
        (replace-var-exps right)))
    (div-exp (left right)
      (div-exp
        (replace-var-exps left)
        (replace-var-exps right)))
    (plus-factor-exp (power)
      (plus-factor-exp
        (replace-var-exps power)))
    (minus-factor-exp (power)
      (minus-factor-exp
        (replace-var-exps power)))
    (power-exp (atom factor)
      (power-exp
        (replace-var-exps atom)
        (replace-var-exps factor)))
    (list-element-exp (list index)
      (list-element-exp
        (replace-var-exps list)
        (replace-var-exps index)))
    (call-exp (function arguments)
      (call-exp
        (replace-var-exps function)
        (replace-var-exps arguments)))
    (arguments-exp (arguments)
      (arguments-exp
        (map replace-var-exps arguments)))
    (var-exp (ID)
      (let ([w (deref (expval->ref (apply-env ID the-scope-env #t)))])
        (if (expval? w)
          (cases expval w
            (proc-val (proc) (var-exp ID))
            (num-val (num) (num-exp num))
            (bool-val (bool) (bool-exp bool))
            (none-val () (none-exp))
            (list-val (lst) (list-const-exp lst))
            (else (report-type-error 'replace-var-exps)))
          (thunk-exp w))))
    (bool-exp (bool) exp1)
    (num-exp (num) exp1)
    (list-exp (list)
      (list-exp
        (map replace-var-exps list)))
    (expressions-exp (expressions)
      (expressions-exp
        (map replace-var-exps expressions)))
    (list-const-exp (values) exp1)
    (thunk-exp (the-thunk) exp1)
    (else (report-type-error 'replace-var-exps))))


;;; Thunk

(define-datatype thunk thunk?
  (a-thunk
   (exp exp?)
   (saved-env env?)))
