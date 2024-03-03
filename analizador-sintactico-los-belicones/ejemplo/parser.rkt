#lang nanopass
(require "lexer.rkt"
          parser-tools/yacc)


; ; 2+3

;     +
;    / \
;   2   +
;      / \
;     3   4

; (+ 2 (+ 3 4))



;     expr
;     / | \
;  expr + expr
;   |      |
;  const  const
;   |      |
;   2      3




(define-struct num (v) #:transparent)
(define-struct bin-op (op v1 v2) #:transparent)

; (bin-op ADD (NUM 2) (NUM 3))

(define hola-parser
    (parser
        [start expr]
        [end EOF]
        [tokens contenedores vacios]
        [error (lambda (msg) (printf "Error de análisis: ~a\n" msg))]
        [grammar
            [const
                [(NUM) (num $1)]]
            [expr
                [(expr ADD expr) (bin-op '+ $1 $3)]
                [(expr MUL expr) (bin-op '* $1 $3)]
                [(const)        $1]]]))

; expr    :: const | expr + expr | expr * expr
; const   :: 0 | 1 | 2 | 3 ... | 9



(define (lex-this lexer input) (lambda () (lexer input)))

(define (parsea in)
        (let ([in-s (open-input-string in)])
        (hola-parser (lex-this jelly-lex in-s))))