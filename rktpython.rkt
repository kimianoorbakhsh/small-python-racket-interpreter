#lang racket

(require (except-in (lib "eopl.ss" "eopl") exp))
(require "store.rkt")
(require "env.rkt")
(require "exp.rkt")
(require "expval.rkt")
(require "lexer.rkt")
(require "parser.rkt")
(require "errors.rkt")
(require "proc.rkt")
(require "typing.rkt")

(provide evaluate)
(define (evaluate file-name)
  (if (file-exists? file-name)
    (begin
      (initialize-typing!)
      (initialize-store!)
      (initialize-global-env!)
      (initialize-scope-env!)
      (let* ([code (file->string file-name)]
             [input (open-input-string code)]
             [lexer (lex-this rktpython-lexer input)]
             [program (rktpython-parser lexer)])
        (when is-checked
          (type-of program))
        (value-of program)))
    (report-file-does-not-exist file-name)))

(define (lex-this lexer input)
  (lambda () (lexer input)))

(define (value-of exp1)
  (cases exp exp1
    (statements-exp (statements)
      (let loop ([statements statements])
        (if (null? statements)
          (void-val)
          (let ([val1 (value-of (car statements))])
            (cases expval val1
              (break-val () val1)
              (continue-val () val1)
              (return-val (val) val1)
              (else (loop (cdr statements))))))))
    (pass-exp ()
      (void-val))
    (break-exp ()
      (break-val))
    (continue-exp ()
      (continue-val))
    (assignment-exp (lhs rhs)
      (cases exp lhs
        (assignment-lhs-exp (ID dtype)
          (assign ID rhs))
        (else (report-type-error 'value-of))))
    (return-stmt-exp (exp1)
      (return-val (value-of exp1)))
    (none-exp () (none-val))
    (global-stmt-exp (ID)
      (let ([ref (expval->ref (apply-env ID the-global-env #t))])
        (update-scope-env! (extend-env ID (ref-val ref) the-scope-env))
        (void-val)))
    (print-stmt-exp (value)
      (pyprint (value-of value))
      (displayln "")
      (void-val))
    (function-def-exp (ID params return-type statements)
      (let* ([thunk-params
        (map
          (lambda (p)
            (cases exp p
              (param-with-default-exp (lhs rhs)
                (cases exp lhs
                  (assignment-lhs-exp (ID dtype)
                    (list ID (a-thunk rhs the-scope-env)))
                  (else (report-type-error 'value-of))))
              (else (report-type-error 'value-of))))
          (exp->params params))]
              [val (ref-val (newref (proc-val (a-proc ID thunk-params statements))))])
        (update-global-env! (extend-env ID val the-global-env))
        (update-scope-env! (extend-env ID val the-scope-env))
        (void-val)))
    (if-stmt-exp (condition true-statements false-statements)
      (if (expval->bool (value-of condition))
        (value-of true-statements)
        (value-of false-statements)))
    (for-stmt-exp (ID iterable statements)
      (let ([iterable (expval->list (value-of iterable))])
        (assign ID (none-exp))
        (let ([iter-ref (expval->ref (apply-env ID the-scope-env #t))])
          (let loop ([iterable iterable])
            (if (null? iterable)
              (void-val)
              (begin
                (setref! iter-ref (car iterable))
                (let ([val (value-of statements)])
                  (cases expval val
                    (break-val () (void-val))
                    (continue-val () (loop (cdr iterable)))
                    (return-val (val) val)
                    (else (loop (cdr iterable)))))))))))
    (or-exp (left right)
      (bool-val (or
        (expval->bool (value-of left))
        (expval->bool (value-of right)))))
    (and-exp (left right)
      (bool-val (and
        (expval->bool (value-of left))
        (expval->bool (value-of right)))))
    (not-exp (exp1)
      (bool-val (not (expval->bool (value-of exp1)))))
    (eq-sum-exp (left right)
      (bool-val
        (cases expval (value-of left)
          (num-val (num)
            (equal? num (expval->num (value-of right))))
          (bool-val (bool)
            (equal? bool (expval->bool (value-of right))))
          (else (report-type-error 'value-of)))))
    (lt-sum-exp (left right)
      (bool-val (< (expval->num (value-of left)) (expval->num (value-of right)))))
    (gt-sum-exp (left right)
      (bool-val (> (expval->num (value-of left)) (expval->num (value-of right)))))
    (add-exp (left right)
      (let ([lval (value-of left)]
            [rval (value-of right)])
        (cases expval lval
          (num-val (num) (num-val (+ num (expval->num rval))))
          (list-val (lst) (list-val (append lst (expval->list rval))))
          (else (report-type-error 'value-of)))))
    (sub-exp (left right)
      (num-val (- (expval->num (value-of left)) (expval->num (value-of right)))))
    (mul-exp (left right)
      (let ([lval (expval->num (value-of left))])
        (num-val
          (if (equal? lval 0)
            0
            (* lval (expval->num (value-of right)))))))
    (div-exp (left right)
      (num-val (/ (expval->num (value-of left)) (expval->num (value-of right)))))
    (plus-factor-exp (power)
      (num-val (expval->num (value-of power))))
    (minus-factor-exp (power)
      (num-val (- (expval->num (value-of power)))))
    (power-exp (atom factor)
      (num-val (expt (expval->num (value-of atom)) (expval->num (value-of factor)))))
    (list-element-exp (list index)
      (list-ref (expval->list (value-of list)) (expval->num (value-of index))))
    (call-exp (function arguments)
      (let ([function (expval->proc (value-of function))]
            [old-scope-env the-scope-env]
            [arguments (exp->arguments arguments)])
        (update-scope-env! (extend-env-with-functions (empty-env #f)))
        (cases procedure function
          (a-proc (p-name params p-body)
            (let loop ([arguments arguments]
                       [params params])
              (cond
                [(and (null? params) (null? arguments)) 88]
                [(null? params) (report-arguments-len-long 'value-of)]
                [(null? arguments)
                  (let ([par-def (car params)])
                    (update-scope-env! (extend-env
                      (car par-def)
                      (ref-val (newref (cadr par-def)))
                      the-scope-env))
                    (loop arguments (cdr params)))]
                [else
                  (let ([par-def (car params)])
                    (update-scope-env! (extend-env
                      (car par-def)
                      (ref-val (newref (a-thunk (car arguments) old-scope-env)))
                      the-scope-env))
                    (loop (cdr arguments) (cdr params)))]))
            (let ([ret-val (value-of p-body)])
              (update-scope-env! old-scope-env)
              (cases expval ret-val
                (void-val () (none-val))
                (return-val (val) val)
                (else (report-type-error 'value-of)))))
          (else (report-type-error 'value-of)))))
    (var-exp (ID)
      (let* ([ref (expval->ref (apply-env ID the-scope-env #t))]
             [w (deref ref)])
        (if (expval? w)
          w
          (let ([thunk-val (value-of-thunk w)])
            (setref! ref thunk-val)
            thunk-val))))
    (bool-exp (bool) (bool-val bool))
    (num-exp (num) (num-val num))
    (list-exp (list) (list-val (map value-of list)))
    (list-const-exp (values) (list-val values))
    (thunk-exp (the-thunk) (value-of-thunk the-thunk))
    (else (report-must-not-reach-here))))

(define (value-of-thunk the-thunk)
  (cases thunk the-thunk
    (a-thunk (exp saved-env)
      (let ([old-scope-env the-scope-env])
        (update-scope-env! saved-env)
        (let ([val (value-of exp)])
          (update-scope-env! old-scope-env)
          val)))))

(define (assign ID rhs)
  (let ([val (apply-env ID the-scope-env #f)]
        [the-thunk (a-thunk (replace-var-exps rhs) the-scope-env)])
      (cases expval val
        (void-val ()
          (let ([ref (newref the-thunk)])
            (when (global-scope? the-scope-env)
              (update-global-env! (extend-env ID (ref-val ref) the-global-env)))
            (update-scope-env! (extend-env ID (ref-val ref) the-scope-env))
            (void-val)))
        (ref-val (ref)
          (setref! ref the-thunk)
          (void-val))
        (else (report-type-error 'assign)))))

(define (pyprint value)
  (cases expval value
    (num-val (num)
      (if (integer? num)
        (display num)
        (display (exact->inexact num))))
    (bool-val (bool)
      (if bool
        (display "True")
        (display "False")))
    (none-val ()
      (display "None"))
    (list-val (lst)
      (display "[")
      (let loop ([lst lst])
        (if (null? lst)
          (void-val)
          (begin
            (pyprint (car lst))
            (when (not (null? (cdr lst)))
              (display ", "))
            (loop (cdr lst)))))
      (display "]"))
    (else (report-type-error 'print))))
