#lang racket

(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         parser-tools/yacc)

(provide (all-defined-out))

;;; Lexer
(define rktpython-lexer
  (lexer
    ("+" (token-plus))
    ("-" (token-minus))
    ("*" (token-mul))
    ("/" (token-div))
    ("**" (token-pow))
    ("<" (token-lt))
    (">" (token-gt))
    ("==" (token-eq))
    ("=" (token-assign))
    ("," (token-comma))
    ("[" (token-lbrack))
    ("]" (token-rbrack))
    ("(" (token-lparanth))
    (")" (token-rparanth))
    ("True" (token-true))
    ("False" (token-false))
    ("None" (token-none))
    ("not" (token-not))
    ("or" (token-or))
    ("and" (token-and))
    (":" (token-colon))
    (";" (token-semicolon))
    ("for" (token-for))
    ("in" (token-in))
    ("break" (token-break))
    ("continue" (token-continue))
    ("if" (token-if)) 
    ("else" (token-else))
    ("def" (token-def))
    ("global" (token-global))
    ("return" (token-return))
    ("pass" (token-pass))
    ("print" (token-print))
    ("checked" (token-checked))
    ("int" (token-int))
    ("float" (token-float))
    ("bool" (token-bool))
    ("list" (token-LIST))
    ("->" (token-to))
    ((:or (:+ (char-range #\0 #\9))
          (:: (:+ (char-range #\0 #\9)) #\. (:+ (char-range #\0 #\9))))
      (token-NUM (string->number lexeme)))
    ((::
      (:or (char-range #\a #\z) (char-range #\A #\Z) #\_)
      (:* (:or (char-range #\a #\z) (char-range #\A #\Z) (char-range #\0 #\9) #\_)))
      (token-ID lexeme))
    (whitespace (rktpython-lexer input-port))
    ((eof) (token-EOF))))


;;; Tokens
(define-tokens num-token (NUM))
(define-tokens id-token (ID))
(define-empty-tokens empty-tokens (plus
                                    minus
                                    mul
                                    div
                                    pow
                                    lt
                                    gt
                                    eq
                                    assign
                                    comma
                                    lbrack
                                    rbrack
                                    lparanth
                                    rparanth
                                    true
                                    false
                                    none
                                    not
                                    or
                                    and
                                    colon
                                    semicolon
                                    for
                                    in
                                    break
                                    continue
                                    if
                                    else
                                    def
                                    global
                                    return
                                    pass
                                    print
                                    checked
                                    int
                                    float
                                    bool
                                    LIST
                                    to
                                    EOF))
