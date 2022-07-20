#lang racket

(require (lib "eopl.ss" "eopl"))
(require "expval.rkt")
(require "store.rkt")
(require "proc.rkt")
(require "errors.rkt")

(provide (all-defined-out))

(define-datatype environment environment?
  (empty-env-record
    (global boolean?))
  (extended-env-record
    (global boolean?)
    (var string?)
    (val expval?)
    (next-env environment?)))

(define (apply-env search-var env with-error)
  (cases environment env
    (empty-env-record (global)
      (if with-error
        (report-no-binding-found 'apply-env search-var)
        (void-val)))
    (extended-env-record (global saved-var saved-val next-env)
      (if (equal? saved-var search-var)
        saved-val
        (apply-env search-var next-env with-error)))
    (else
      (report-invalid-env env))))

(define empty-env empty-env-record)

(define (extend-env var val env)
  (cases environment env
    (empty-env-record (global)
      (extended-env-record global var val env))
    (extended-env-record (global next-var next-val next-env)
      (extended-env-record global var val env))))

(define (extend-env-with-functions env)
  (let loop ([env env]
             [g-env the-global-env])
    (cases environment g-env
      (empty-env-record (global) env)
      (extended-env-record (global var val next-env)
        (cases expval val
          (ref-val (ref)
            (let ([w (deref ref)])
              (if (expval? w)
                (cases expval w
                  (proc-val (proc)
                    (cases procedure proc
                      (a-proc (ID params p-body)
                        (loop (extend-env ID val env) next-env))))
                  (else
                    (loop env next-env)))
                (loop env next-env))))
          (else
            (report-type-error 'extend-env-with-functions)))))))

(define (global-scope? env)
  (cases environment env
    (empty-env-record (global)
      global)
    (extended-env-record (global var val next-env)
      global)))

(define the-global-env 'uninitialized)

(define the-scope-env 'uninitialized)

(define (initialize-global-env!)
  (set! the-global-env (empty-env #t)))

(define (initialize-scope-env!)
  (set! the-scope-env (empty-env #t)))

(define (update-global-env! env)
  (set! the-global-env env))

(define (update-scope-env! env)
  (set! the-scope-env env))
