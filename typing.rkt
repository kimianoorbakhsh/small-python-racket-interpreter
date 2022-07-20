#lang racket

(require (except-in (lib "eopl.ss" "eopl") exp))
(require "errors.rkt")
(require "exp.rkt")

(provide (all-defined-out))

(define is-checked 'uninitialized)

(define (initialize-typing!)
	(begin
		(set! is-checked #f)
		(initialize-the-tenv!)))

(define (enable-checked!)
	(set! is-checked #t))

(define-datatype type type?
	(undefined-type)
	(int-type)
	(float-type)
	(bool-type)
	(list-type)
	(none-type)
	(function-type
		(param-names (list-of string?))
		(param-types (list-of type?))
		(return-type type?)))

(define-datatype type-environment type-environment?
	(empty-tenv-record)
	(extended-tenv-record
		(var string?)
		(type type?)
		(next-tenv type-environment?)))

(define (apply-tenv search-var tenv)
	(cases type-environment tenv
		(empty-tenv-record ()
			(report-no-binding-found 'apply-tenv search-var))
		(extended-tenv-record (saved-var saved-type next-tenv)
			(if (equal? search-var saved-var)
				saved-type
				(apply-tenv search-var next-tenv)))))

(define empty-tenv empty-tenv-record)
(define extend-tenv extended-tenv-record)

(define (list-join l sep)
	(if (null? l)
		'()
		(let loop ([acc (list (car l))]
							 [l (cdr l)])
			(if (null? l)
				(reverse acc)
				(loop
					(cons (car l) (cons sep acc))
					(cdr l))))))

(define (type-to-external-form t)
	(cases type t
		(undefined-type () 'undefined)
		(int-type () 'int)
		(float-type () 'float)
		(bool-type () 'bool)
		(list-type () 'list)
		(none-type () 'None)
		(function-type (param-names param-types return-type)
			(append
				(list-join
					(map type-to-external-form param-types)
					'*)
				(list '-> (type-to-external-form return-type))))))

(define (report-unequal-types t1 t2 exp)
  (eopl:error 'check-equal-type!
    "Expected type of expression ~a to be ~a, but got ~a"
    exp
    (type-to-external-form t2)
    (type-to-external-form t1)))

(define (check-equal-type! t1 t2 exp)
	(when (not
		(or
			(equal? t1 (undefined-type))
			(equal? t2 (undefined-type))
			(equal? t1 t2)))
		(report-unequal-types t1 t2 exp)))

(define (report-type-not-in t types exp)
	(eopl:error 'check-type-in!
		"Expected type of expression ~a to be one of ~a, but got ~a"
    exp
		(map type-to-external-form types)
		(type-to-external-form t)))

(define (check-type-in! t types exp)
	(when (not (member t (cons (undefined-type) types)))
		(report-type-not-in t types exp)))

(define (report-not-a-function f-type function)
	(eopl:error 'type-of
		"Callee not a function type:~%~s~%had type ~s"
		function
		(type-to-external-form f-type)))

(define the-tenv 'uninitialized)

(define (initialize-the-tenv!)
  (set! the-tenv (empty-tenv)))

(define (type-of exp1)
	(cases exp exp1
		(statements-exp (statements)
			(begin
				(map type-of statements)
				(none-type)))
		(assignment-exp (lhs rhs)
			(cases exp lhs
				(assignment-lhs-exp (ID dtype)
					(assign-type ID dtype rhs))
				(else (report-type-error 'type-of))))
		(return-stmt-exp (exp1)
			(type-of exp1))
		(none-exp ()
			(none-type))
		(function-def-exp (ID params return-type statements)
			(let* ([param-types
				(map
					(lambda (p)
						(cases exp p
							(param-with-default-exp (lhs rhs)
								(cases exp lhs
									(assignment-lhs-exp (ID dtype)
										dtype)
									(else (report-type-error 'type-of))))
              (else (report-type-error 'type-of))))
					(exp->params params))]
						 [param-names
				(map
					(lambda (p)
						(cases exp p
							(param-with-default-exp (lhs rhs)
								(cases exp lhs
									(assignment-lhs-exp (ID dtype)
										ID)
									(else (report-type-error 'type-of))))
              (else (report-type-error 'type-of))))
					(exp->params params))]
						 [tf (function-type param-names param-types return-type)])
				(set! the-tenv (extend-tenv ID tf the-tenv))
				(none-type)))
		(if-stmt-exp (condition true-statements false-statements)
			(begin
				(check-equal-type! (type-of condition) (bool-type) condition)
				(type-of true-statements)
				(type-of false-statements)
				(none-type)))
		(for-stmt-exp (ID iterable statements)
			(begin
				(type-of iterable)
				(assign-type ID (undefined-type) (none-exp))
				(type-of statements)
				(none-type)))
		(or-exp (left right)
			(begin
				(check-equal-type! (type-of left) (bool-type) left)
				(check-equal-type! (type-of right) (bool-type) right)
				(bool-type)))
		(and-exp (left right)
			(begin
				(check-equal-type! (type-of left) (bool-type) left)
				(check-equal-type! (type-of right) (bool-type) right)
				(bool-type)))
		(not-exp (exp1)
			(begin
				(check-equal-type! (type-of exp1) (bool-type) exp1)
				(bool-type)))
		(eq-sum-exp (left right)
			(begin
				(check-type-in! (type-of left) (list (bool-type) (int-type) (float-type)) left)
				(check-equal-type! (type-of right) (type-of left) right)
				(bool-type)))
		(lt-sum-exp (left right)
			(begin
				(check-type-in! (type-of left) (list (int-type) (float-type)) left)
				(check-equal-type! (type-of right) (type-of left) right)
				(bool-type)))
		(gt-sum-exp (left right)
			(begin
				(check-type-in! (type-of left) (list (int-type) (float-type)) left)
				(check-equal-type! (type-of right) (type-of left) right)
				(bool-type)))
		(add-exp (left right)
			(begin
				(check-type-in! (type-of left) (list (int-type) (float-type) (list-type)) left)
				(check-equal-type! (type-of right) (type-of left) right)
				(type-of left)))
		(sub-exp (left right)
			(begin
				(check-type-in! (type-of left) (list (int-type) (float-type)) left)
				(check-equal-type! (type-of right) (type-of left) right)
				(type-of left)))
		(mul-exp (left right)
			(begin
				(check-type-in! (type-of left) (list (int-type) (float-type)) left)
				(check-equal-type! (type-of right) (type-of left) right)
				(type-of left)))
		(div-exp (left right)
			(begin
				(check-type-in! (type-of left) (list (int-type) (float-type)) left)
				(check-equal-type! (type-of right) (type-of left) right)
				(type-of left)))
		(plus-factor-exp (power)
			(begin
				(check-type-in! (type-of power) (list (int-type) (float-type)) power)
				(type-of power)))
		(minus-factor-exp (power)
			(begin
				(check-type-in! (type-of power) (list (int-type) (float-type)) power)
				(type-of power)))
		(power-exp (atom factor)
			(begin
				(check-type-in! (type-of atom) (list (int-type) (float-type)) atom)
				(check-type-in! (type-of factor) (list (int-type) (float-type)) factor)
				(float-type)))
		(list-element-exp (list index)
			(begin
				(check-equal-type! (type-of list) (list-type) list)
				(check-equal-type! (type-of index) (int-type) index)
				(undefined-type)))
		(call-exp (function arguments)
			(let ([f-type (type-of function)]
						[arg-types (map type-of (exp->arguments arguments))])
				(cases type f-type
					(function-type (param-names param-types return-type)
						(let loop ([arguments arguments]
											 [arg-types arg-types]
											 [param-names param-names]
											 [param-types param-types])
							(cond
								[(null? arg-types) 88]
								[(null? param-types) (report-arguments-len-long 'type-of)]
								[else
									(begin
										(check-equal-type!
											(car arg-types)
											(car param-types)
											(car arguments))
										(loop (cdr arguments) (cdr arg-types) (cdr param-names) (cdr param-types)))]))
						return-type)
					(else (report-not-a-function f-type function)))))
		(var-exp (ID)
			(apply-tenv ID the-tenv))
		(bool-exp (bool)
      (bool-type))
    (num-exp (num)
      (if (integer? num)
        (int-type)
        (float-type)))
    (list-exp (list)
      (list-type))
    (list-const-exp (values)
      (list-type))
		(else (none-type))))

(define (assign-type ID dtype rhs)
	(begin
		(set! the-tenv (extend-tenv ID dtype the-tenv))
		(check-equal-type! (type-of rhs) dtype rhs)
		(none-type)))
