#lang racket

(require (lib "eopl.ss" "eopl"))

(provide (all-defined-out))

(define is-checked 'uninitialized)

(define (initialize-typing!)
	(set! is-checked #f))

(define (enable-checked!)
	(set! is-checked #t))

(define-datatype type type?
	(undefined-type)
	(int-type)
	(float-type)
	(bool-type)
	(list-type)
	(none-type))
