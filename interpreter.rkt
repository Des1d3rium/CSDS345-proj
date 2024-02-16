#lang racket

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
            ;[andrej]: I'm not sure if a hashtable is necessary, but it's a good idea to have a lookup function.
            [(not (null? (lookup state return))) ]
            [else (statementHandler (cdr prog) (M_state (car prog) state))]
            )))

(define lookup
    (lambda (state key)
        (cond
            [(null? state) (error "key not found")]
            [(eq? (car (car state)) key) (cdr (car state))]
            [else (lookup (cdr state) key)])
    )
)

(define M_state
    (lambda (statement state)
        (cond
            [(isReturn? statement)          (return statement)]
            [(isDeclaration? statement)     (declare statement)]
            [(isAssignment? statement)      (assign statement)]
            [(isIfStatement? statement)     (ifImp statement)]
            [(isWhileStatement? statement)  (whileImp statement)]
            [else (error "Invalid statement")]
    )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;
;  Functions for each statement
;
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define isReturn?
    (lambda (statement)
        (eq? (car statement) 'return)))

(define return cadr) ; a bit of a hack but who's counting

(define isDeclaration?
    (lambda (statement)
        (eq? (car statement) 'declare)))

(define declare
    (lambda (statement)
        (set (cadr statement) 0)))

(define isAssignment?
    (lambda (statement)
        (eq? (car statement) 'assign)))

(define assign
    (lambda (statement)
        (set (cadr statement) (M_value (caddr statement)))))

(define isIfStatement?
    (lambda (statement)
        (eq? (car statement) 'if)))

(define ifImp
    (lambda (statement)
        (if (M_boolean (cadr statement))
            (statementHandler (caddr statement))
            (statementHandler (cadddr statement)))))

(define isWhileStatement?
    (lambda (statement)
        (eq? (car statement) 'while)))

(define whileImp
    (lambda (statement)
        (if (M_boolean (cadr statement))
            (begin (statementHandler (caddr statement))
                   (whileImp statement))
            '())))

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
(define M_boolean 
    (lambda (statement)
        (cond
            [(eq? (car statement) '>)  (> (M_value (cadr statement)) (M_value (caddr statement)))]
            [(eq? (car statement) '>=) (>= (M_value (cadr statement)) (M_value (caddr statement)))]
            [(eq? (car statement) '<)  (< (M_value (cadr statement)) (M_value (caddr statement)))]
            [(eq? (car statement) '<=) (<= (M_value (cadr statement)) (M_value (caddr statement)))]
            [(eq? (car statement) '=)  (= (M_value (cadr statement)) (M_value (caddr statement)))]
            [(eq? (car statement) '!=) (not (= (M_value (cadr statement)) (M_value (caddr statement))))]
            [else (error "")]
        )))


(parser "../CSDS345/proj1/test1.txt")
(statementHandler (parser "../CSDS345/proj1/test1.txt") '())
