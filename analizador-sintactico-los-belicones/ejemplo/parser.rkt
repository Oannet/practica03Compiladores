#lang nanopass
(require "lexer.rkt"
          parser-tools/yacc)

(define-struct array-access (id index) #:transparent)
(define-struct assign-add (id expr) #:transparent)
(define-struct assign-sub (id expr) #:transparent)
(define-struct inc (id) #:transparent)
(define-struct dec (id) #:transparent)
(define-struct multiple-decl (vars) #:transparent)

; ESTRUCTURAS PARA LOS NODOS DEL ARBOL
(define-struct id (i) #:transparent) 
(define-struct num (n) #:transparent)
(define-struct bool (b) #:transparent)
(define-struct return (exp) #:transparent)
(define-struct length (exp) #:transparent)
(define-struct arg (id tipo) #:transparent)
(define-struct un-expr (op e) #:transparent)
(define-struct if-corto (g t e) #:transparent)                
(define-struct call (nombre arg) #:transparent)
(define-struct my-while (exp then) #:transparent)
(define-struct bin-expr (op o1 o2) #:transparent)
(define-struct main (instrucciones) #:transparent)
(define-struct my-if (exp then else) #:transparent)
(define-struct arreglo (id tipo tamano) #:transparent)           
(define-struct procedimiento (nombre argumentos tipo instrucciones) #:transparent)

(define jelly-parser
  (parser
    [start program]    
    [end EOF]       
    [tokens contenedores vacios] 
    [error void]    
    
    [precs  (right EQ += -= *= /=) 
            (nonassoc NEQ < <= > >=)
            (left ADD -)
            (left MUL / MOD) 
            (nonassoc NOT)
            (left AND)
            (left OR)]

    [grammar
        [program
            [(main func-list) (main $1 $2)]]
        [main
            [(MAIN LBr decl-var-list expr RBr) (main $3 $4)]]
        [func-list
            [() '()]
            [(func func-list) (cons $1 $2)]]
        [func
            [(ID LP arg-list RP type LBr expr RBr) (procedimiento $1 $3 $5 $7)]]
        [arg-list
            [() '()]
            [(ID) (list (arg $1))]
            [(ID COM arg-list) (cons (arg $1) $3)]]
        [type
            [(INT) 'INT]
            [(BOOL) 'BOOL]]
        [decl-var-list
            [() '()]
            [(decl-var decl-var-list) (cons $1 $2)]]
        [expr
            [(ID) (id $1)]
            [(NUM) (num $1)]
            [(expr ADD expr) (bin-expr '+ $1 $3)]
            [(expr SUB expr) (bin-expr '- $1 $3)]
            [(expr MUL expr) (bin-expr '* $1 $3)]
            [(expr DIV expr) (bin-expr '/ $1 $3)]
            [(expr MOD expr) (bin-expr '% $1 $3)]
            [(NOT expr) (un-expr 'NOT $2)]
            [(expr AND expr) (bin-expr 'AND $1 $3)]
            [(expr OR expr) (bin-expr 'OR $1 $3)]
            [(expr EQ expr) (bin-expr 'EQ $1 $3)]
            [(expr NEQ expr) (bin-expr 'NEQ $1 $3)]
            [(expr LT expr) (bin-expr '< $1 $3)]
            [(expr LE expr) (bin-expr '<= $1 $3)]
            [(expr GT expr) (bin-expr '> $1 $3)]
            [(expr GE expr) (bin-expr '>= $1 $3)]
            [(ID LBr expr RBr) (array-access $1 $3)]
            [(LEN LP ID RP) (length $3)]
            [(ID ADD= expr) (assign-add $1 $3)]
            [(ID SUB= expr) (assign-sub $1 $3)]
            [(ID INC) (inc $1)]
            [(ID DEC) (dec $1)]
            [(ID COM ID COM ID EQ expr) (multiple-decl (list $1 $3 $5 $7))]
        ]
    ]
  )
)


(define (lex-this lexer input) (lambda () (lexer input)))

(define (parsea in)
        (let ([in-s (open-input-string in)])
        (hola-parser (lex-this jelly-lex in-s))))