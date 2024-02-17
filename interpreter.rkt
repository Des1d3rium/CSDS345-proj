#lang racket

(require "simpleParser.rkt")

;recursively running the program. Each statement is divided into an element of a list by parser.
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
        (cond
            ;[Daniel]: it is normal for return not to find a value, so just return null here. Error should be handled
            ;in declare and assign separately if they find null.  
            [(null? state) null]
            [(eq? (car (car state)) key) (car (cdr (car state)))]
            [else (lookup (cdr state) key)])
    )
)

(define M_state
    (lambda (statement state)
        (cond
            [(isReturn? statement)          (return statement state)]
            [(isDeclaration? statement)     (declare statement state)]
            [(isAssignment? statement)      (assign statement state)]
            [(isIfStatement? statement)     (ifImp statement state)]
            [(isWhileStatement? statement)  (whileImp statement state)]
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

;[Daniel]: In my view, the return function doesn't 'return' the value. Instead, it store the value into state 
; and StatementHandler could return it in the next recurively call.  
(define return 
    (lambda (statement state)
        (cons state (cons 'return M_value(statement state)))))

(define isDeclaration?
    (lambda (statement)
        (eq? (car statement) 'declare)))

(define declare
    (lambda (statement)
        (set (cadr statement) 0)))

(define isAssignment?
    (lambda (statement)
        (eq? (car statement) 'var)))

(define assign
    (lambda (statement state)
        (set (cadr statement) (M_value (caddr statement) state))))

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

(define M_value
    (lambda (statement state)
        (cond
            [(number? statement)        statement]
            [(not (null? (lookup statement state))) (lookup statement state)];if it is a variable 
            [(eq? (car statement) '+)   (+ (M_value (cadr statement) state) (M_value (caddr statement) state))]
            [(eq? (car statement) '-)   (- (M_value (cadr statement) state) (M_value (caddr statement) state))]
            [(eq? (car statement) '*)   (* (M_value (cadr statement) state) (M_value (caddr statement) state))]
            [(eq? (car statement) '/)   (quotient (M_value (cadr statement) state) (M_value (caddr statement) state))]
            [(eq? (car statement) '%)   (remainder (M_value (cadr statement) state) (M_value (caddr statement) state))]
            [else (error "not a value")]
        )))

;TODO: we shouldn't return #t or #f but true or false here.
(define M_boolean 
    (lambda (statement state)
        (cond
            [(eq? (car statement) '>)  (> (M_value (cadr statement) state) (M_value (caddr statement) state))]
            [(eq? (car statement) '>=) (>= (M_value (cadr statement) state) (M_value (caddr statement) state))]
            [(eq? (car statement) '<)  (< (M_value (cadr statement) state) (M_value (caddr statement) state))]
            [(eq? (car statement) '<=) (<= (M_value (cadr statement) state) (M_value (caddr statement) state))]
            [(eq? (car statement) '=)  (= (M_value (cadr statement) state) (M_value (caddr statement) state))]
            [(eq? (car statement) '!=) (not (= (M_value (cadr statement) state) (M_value (caddr statement) state)))]
            [(eq? (car statement) '&&) (and (M_boolean (cadr statement) state) (M_boolean (caddr statement) state))]
            [(eq? (car statement) '||) (or (M_boolean (cadr statement) state) (M_boolean (caddr statement) state))]
            [else (error "not a boolean")]
        )))


(parser "../CSDS345/proj1/test1.txt")
(statementHandler (parser "../CSDS345/proj1/test1.txt") '())
