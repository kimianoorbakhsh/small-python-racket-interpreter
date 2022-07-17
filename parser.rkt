#lang racket

(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         parser-tools/yacc)
(require "lexer.rkt")
(require "exp.rkt")

(provide (all-defined-out))

;;; Parser
(define rktpython-parser
  (parser
    (start program)
    (end EOF)
    (error (lambda (tok-ok? tok-name tok-value) (printf "Error in token ~s (~s)\n" tok-name tok-value)))
    (tokens num-token id-token empty-tokens)
    (grammar
      (program
        ((statements) $1)
        ((checked statements) $2)) ;;; TODO: store somewhere that the program is checked!
      (statements
        ((statement semicolon) (statements-exp (list $1)))
        ((statements statement semicolon) (statements-exp (append (exp->statements $1) (list $2)))))
      (statement
        ((compound-stmt) $1)
        ((simple-stmt) $1))
      (simple-stmt
        ((assignment) $1)
        ((global-stmt) $1)
        ((return-stmt) $1)
        ((print-stmt) $1)
        ((pass) (pass-exp))
        ((break) (break-exp))
        ((continue) (continue-exp)))
      (compound-stmt
        ((function-def) $1)
        ((if-stmt) $1)
        ((for-stmt) $1))
      (assignment
        ((assignment-lhs assign expression) (assignment-exp $1 $3)))
      (assignment-lhs
        ((ID) (assignment-lhs-exp $1 "undefined"))
        ((ID colon type) (assignment-lhs-exp $1 (exp->type $3))))
      (type
        ((int) (type-exp "int"))
        ((float) (type-exp "float"))
        ((bool) (type-exp "bool"))
        ((LIST) (type-exp "list"))
        ((none) (type-exp "None")))
      (return-stmt
        ((return) (return-stmt-exp (none-exp)))
        ((return expression) (return-stmt-exp $2)))
      (global-stmt
        ((global ID) (global-stmt-exp $2)))
      (print-stmt
        ((print lparanth atom rparanth) (print-stmt-exp $3)))
      (function-def
        ((def ID lparanth params rparanth return-type statements) (function-def-exp $2 $4 (exp->type $6) $7))
        ((def ID lparanth rparanth return-type statements) (function-def-exp $2 (params-exp null) (exp->type $5) $6)))
      (return-type
        ((colon) (type-exp "undefined"))
        ((colon minus gt type) $4))
      (params
        ((param-with-default) (params-exp (list $1)))
        ((params comma param-with-default) (params-exp (append (exp->params $1) (list $3)))))
      (param-with-default
        ((assignment-lhs assign expression) (param-with-default-exp $1 $3)))
      (if-stmt
        ((if expression colon statements else-block) (if-stmt-exp $2 $4 $5)))
      (else-block
        ((else colon statements) $3))
      (for-stmt
        ((for ID in expression colon statements) (for-stmt-exp $2 $4 $6)))
      (expression
        ((disjunction) $1))
      (disjunction
        ((conjunction) $1)
        ((disjunction or conjunction) (or-exp $1 $3)))
      (conjunction
        ((inversion) $1)
        ((conjunction and inversion) (and-exp $1 $3)))
      (inversion
        ((not inversion) (not-exp $2))
        ((comparison) $1))
      (comparison
        ((eq-sum) $1)
        ((lt-sum) $1)
        ((gt-sum) $1)
        ((sum) $1))
      (eq-sum
        ((sum eq sum) (eq-sum-exp $1 $3)))
      (lt-sum
        ((sum lt sum) (lt-sum-exp $1 $3)))
      (gt-sum
        ((sum gt sum) (gt-sum-exp $1 $3)))
      (sum
        ((sum plus term) (add-exp $1 $3))
        ((sum minus term) (sub-exp $1 $3))
        ((term) $1))
      (term
        ((term mul factor) (mul-exp $1 $3))
        ((term div factor) (div-exp $1 $3))
        ((factor) $1))
      (factor
        ((plus power) (plus-factor-exp $2))
        ((minus power) (minus-factor-exp $2))
        ((power) $1))
      (power
        ((atom pow factor) (power-exp $1 $3))
        ((primary) $1))
      (primary
        ((atom) $1)
        ((primary lbrack expression rbrack) (list-element-exp $1 $3))
        ((primary lparanth rparanth) (call-exp $1 (arguments-exp null)))
        ((primary lparanth arguments rparanth) (call-exp $1 $3)))
      (arguments
        ((expression) (arguments-exp (list $1)))
        ((arguments comma expression) (arguments-exp (append (exp->arguments $1) (list $3)))))
      (atom
        ((ID) (var-exp $1))
        ((true) (bool-exp #t))
        ((false) (bool-exp #f))
        ((none) (none-exp))
        ((NUM) (num-exp $1))
        ((list) $1))
      (list
        ((lbrack expressions rbrack) (list-exp (exp->expressions $2)))
        ((lbrack rbrack) (list-exp null)))
      (expressions
        ((expressions comma expression) (expressions-exp (append (exp->expressions $1) (list $3))))
        ((expression) (expressions-exp (list $1)))))))
