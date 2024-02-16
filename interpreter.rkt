#lang Racket
(require "simpleParser.rkt")

;recursively running the program. Each statement is divided into an element of a list by parser.
(define statementHandler
    (lambda (prog state)
        (cond   
            ;the code shall exit when reach to the end of program (if there is no return)   
            [(null? prog) '()]
            ;if the program demand return that 'return' is assigned some value in state instead of a default null,
            ;the program should return it.  
            ;TODO: implement (lookup state key). We also need a set function for state, or namely a hashtable
            [(not (null? (lookup state return))) ]
            [else (statementHandler (cdr prog) (M_state (car prog) state))]
            )))

(define M_state
    (lambda (statement state)
        (cond
            ;TODO: implement isReturn?, return, isDeclaration?, declare, isAssignment? assign, isIfStatement? ifImp, isWhileStatement?, whileImp.
            [(isReturn? statement)          (return statement)]
            [(isDeclaration? statement)     (declare statement)]
            [(isAssignment? statement)      (assign statement)]
            [(isIfStatement? statement)     (ifImp statement)]
            [(isWhileStatement? statement)  (whileImp statement)]
            [else (error "Invalid statement")]
    )))

;TODO: implement all numeric operator here
(define M_value
    (lambda (statement)
        (cond
            [(number? statement)        statement]
            [(eq? (car statement) '+)   (+ (M_value (cadr statement)) (M_value (caddr statement)))]
            [(eq? (car statement) '-)   (- (M_value (cadr statement)) (M_value (caddr statement)))]
            [(eq? (car statement) '*)   (* (M_value (cadr statement)) (M_value (caddr statement)))]
            [(eq? (car statement) '/)   (quotient (M_value (cadr statement)) (M_value (caddr statement)))]
            [(eq? (car statement) '%)   (remainder (M_value (cadr statement)) (M_value (caddr statement)))]
            [else (error "")]
        )))
;TODO: implement all logical operator here
(define M_boolean)
