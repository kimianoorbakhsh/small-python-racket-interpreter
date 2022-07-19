#lang racket

(require (lib "eopl.ss" "eopl"))
(require "expval.rkt")
(require "store.rkt")
(require "proc.rkt")
(require "errors.rkt")

(provide (all-defined-out))

(define-datatype environment env?
  (empty-env)
  (extend-env
    (var string?)
    (val expval?)
    (saved-env env?)))

(define (apply-env search-var env with-error)
  (cases environment env
    (empty-env ()
      (if with-error
        (report-no-binding-found 'apply-env search-var)
        (void-val)))
    (extend-env (saved-var saved-val saved-env)
      (if (equal? saved-var search-var)
        saved-val
        (apply-env search-var saved-env with-error)))
    (else
      (report-invalid-env env))))

(define (init-env is-global)
  (extend-env "is-global" (bool-val is-global) (empty-env)))

(define (extend-env-with-functions env)
  (let loop ([env env]
              [g-env the-global-env])
    (cases environment g-env
      (empty-env () env)
      (extend-env (var val saved-env)
        (cases expval val
          (ref-val (ref)
            (let ([w (deref ref)])
              (if (expval? w)
                (cases expval w
                  (proc-val (proc)
                    (cases procedure proc
                      (a-proc (ID params p-body)
                        (loop (extend-env ID val env) saved-env))))
                  (else
                    (loop env saved-env)))
                (loop env saved-env))))
          (bool-val (bool) (loop env saved-env)) ;For is-global in env with holds a bool-val
          (else
            (report-type-error 'extend-env-with-functions)))))))

(define global-scope?
  (lambda (env)
    (expval->bool (apply-env "is-global" env #t))))

(define the-global-env 'uninitialized)

(define the-scope-env 'uninitialized)

(define (initialize-global-env!)
  (set! the-global-env (init-env #t)))

(define (initialize-scope-env!)
  (set! the-scope-env (init-env #t)))

(define (update-global-env! env)
  (set! the-global-env env))

(define (update-scope-env! env)
  (set! the-scope-env env))
