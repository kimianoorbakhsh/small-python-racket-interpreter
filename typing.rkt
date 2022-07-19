#lang racket

(provide (all-defined-out))

(define is-checked 'uninitialized)

(define (initialize-typing!)
	(set! is-checked #f))

(define (enable-checked!)
	(set! is-checked #t))
