#lang racket

(require "simpleParser.rkt")

(define statementHandler
    (lambda (prog state)
        (cond   
            ;the code shall exit when reach to the end of program (if there is no return)   
            ;[(null? prog) '()]
            ;if the program demand return that 'return' is assigned some value in state instead of a default null,
            ;the program should return it.  
            [(not (null? (lookup state 'return))) (lookup state 'return)]
            [else (statementHandler (cdr prog) (M_state (car prog) state))]
            )))

(define lookup
    (lambda (state key)
        (begin
            (display "Looking up key: ") (display key) (newline)
            (if (hash-has-key? state key)
                (hash-ref state key)
                '()))))

(define M_state
    (lambda (statement state)
        (begin
            (display "Running M_state with statement: ") (display statement) (newline)
            (cond
                [(isReturn? statement)          (return statement state)]
                [(isDeclaration? statement)     (declare statement state)]
                [(isAssignment? statement)      (assign statement state)]
                [(isIfStatement? statement)     (ifImp statement state)]
                [(isWhileStatement? statement)  (whileImp statement state)]
                [else (error "Invalid statement")]
            ))))

(define return 
    (lambda (statement state)
        (begin
            (display "Running return with statement: ") (display statement) (newline)
            (hash-set state 'return (M_value (cadr statement) state)))))

(define declare
    (lambda (statement state)
        (begin
            (display "Running declare with statement: ") (display statement) (newline)
            (hash-set state (cadr statement) 0))))

(define assign
    (lambda (statement state)
        (begin
            (display "Running assign with statement: ") (display statement) (newline)
            (hash-set state (cadr statement) (M_value (caddr statement) state)))))

(define ifImp
    (lambda (statement state)
        (begin
            (display "Running ifImp with statement: ") (display statement) (newline)
            (if (M_boolean (cadr statement) state)
                (statementHandler (caddr statement) state)
                (statementHandler (cadddr statement) state)))))

(define whileImp
    (lambda (statement state)
        (begin
            (display "Running whileImp with statement: ") (display statement) (newline)
            (if (M_boolean (cadr statement) state)
                (begin (statementHandler (caddr statement) state)
                       (whileImp statement state))
                '()))))

(define M_value
    (lambda (statement state)
        (begin
            (display "Running M_value with statement: ") (display statement) (newline)
            (cond
                [(number? statement)        statement]
                [(symbol? statement)        (lookup state statement)]
                [(eq? (car statement) '+)   (+ (M_value (cadr statement) state) (M_value (caddr statement) state))]
                [(eq? (car statement) '-)   (- (M_value (cadr statement) state) (M_value (caddr statement) state))]
                [(eq? (car statement) '*)   (* (M_value (cadr statement) state) (M_value (caddr statement) state))]
                [(eq? (car statement) '/)   (quotient (M_value (cadr statement) state) (M_value (caddr statement) state))]
                [(eq? (car statement) '%)   (remainder (M_value (cadr statement) state) (M_value (caddr statement) state))]
                [else (lookup state statement)]
            ))))

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

;[Daniel]: In my view, the return function doesn't 'return' the value. Instead, it store the value into state 
; and StatementHandler could return it in the next recurively call.  

(define isDeclaration?
    (lambda (statement)
        (and (list? statement) (eq? (car statement) 'var))))

(define isAssignment?
    (lambda (statement)
        (eq? (car statement) '=)))

(define isIfStatement?
    (lambda (statement)
        (and (list? statement) (eq? (car statement) 'if))))

(define isWhileStatement?
    (lambda (statement)
        (and (list? statement) (eq? (car statement) 'while))))

;TODO: we shouldn't return #t or #f but true or false here.
(define M_boolean 
    (lambda (statement state)
        (cond
            [(eq? (car statement) '>)  (> (M_value (cadr statement) state) (M_value (caddr statement) state))]
            [(eq? (car statement) '>=) (>= (M_value (cadr statement) state) (M_value (caddr statement) state))]
            [(eq? (car statement) '<)  (< (M_value (cadr statement) state) (M_value (caddr statement) state))]
            [(eq? (car statement) '<=) (<= (M_value (cadr statement) state) (M_value (caddr statement) state))]
            [(eq? (car statement) '=)  (assign (cadr statement) (M_value (caddr statement) state)) (M_value (cadr statement) state)]
            [(eq? (car statement) '!=) (not (= (M_value (cadr statement) state) (M_value (caddr statement) state)))]
            [(eq? (car statement) '&&) (and (M_boolean (cadr statement) state) (M_boolean (caddr statement) state))]
            [(eq? (car statement) '||) (or (M_boolean (cadr statement) state) (M_boolean (caddr statement) state))]
            [else (error "not a boolean")]
        )))


(parser "../CSDS345/proj1/test1.txt")
(statementHandler (parser "../CSDS345/proj1/test1.txt") (hash))