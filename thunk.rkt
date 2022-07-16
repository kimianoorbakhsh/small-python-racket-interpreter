#lang racket

(require (lib "eopl.ss" "eopl"))
(require "exp.rkt")
(require "env.rkt")
(require "errors.rkt")

(provide (all-defined-out))

(define-datatype thunk thunk?
  (a-thunk
   (exp exp?)
   (saved-env env?)))

(define (value-of-thunk the-thunk)
  (cases thunk the-thunk
    (a-thunk (exp saved-env)
      (let ([old-scope-env the-scope-env])
        (set! the-scope-env saved-env)
        (let ([val (value-of exp)])
          (set! the-scope-env old-scope-env)
          val)))))


