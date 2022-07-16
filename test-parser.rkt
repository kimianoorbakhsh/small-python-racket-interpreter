#lang racket

(require "lexer.rkt")
(require "parser.rkt")

(define lex-this (lambda (lexer input) (lambda () (lexer input))))
(define the-lexer (lex-this rktpython-lexer (open-input-string "a = 2; a = a - 3; if a > 0: b = a + 5; else: pass;;")))
(let ((parsed (rktpython-parser the-lexer))) parsed)
