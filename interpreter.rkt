#lang racket

(require "simpleParser.rkt")

(define statementHandler
    (lambda (prog state)
        (cond   
            [(null? prog) state]
            [else (statementHandler (cdr prog) (M_state (car prog) state))]
            )))

(define lookup
    (lambda (state key)
            (display "Looking up key: ") (display key) (newline)
            (cond
                [(null? state) null]
                [(eq? (caar state) key) (cdar state)]
                [else (lookup (cdr state) key)]
            )
))

(define M_state
    (lambda (statement state)
            (display "Running M_state with statement: ") (display statement) (newline)
            (display "State: ") (display state) (newline)
            (cond
                [(isReturn? statement)          (return statement state)]
                [(isDeclaration? statement)     (declare statement state)]
                [(isAssignment? statement)      (assign statement state)]
                [(isIfStatement? statement)     (ifImp statement state)]
                [(isWhileStatement? statement)  (whileImp statement state)]
                [else (error "Invalid statement")]
            )
))

(define return 
    (lambda (statement state)
            (display "Running return with statement: ") (display statement) (newline)
            (cons state (cons 'return M_value(statement state)))))

(define declare
    (lambda (statement state)
            (display "Running declare with statement: ") (display statement) (newline)
            ; If the variable has a value upon declaration, evaluate it and store it in the state.
            ; If it's just declared, store it in the state with a 0 value.
            ; If the variable already exists, throw an error.
            (if (not (null? (lookup state (cadr statement))))
                (error "Variable already declared")
                (if (null? (cddr statement))
                    (cons (cons (cadr statement) 0) state)
                    (cons (cons (cadr statement) (M_value (caddr statement) state)) state)
                ))))

(define (assign statement state)
    (display "Running assign with statement: ") (display statement) (newline)
    ; Lookup the variable in the state and change its value to the evaluated value.
    ; If the variable doesn't exist, throw an error.
    ; If the variable already exists, update its value.
    (let ((var (cadr statement))
                (val (M_value (caddr statement) state)))
        (if (null? (lookup state var))
                (error "Variable not declared")
                (map (lambda (pair) (if (eq? (car pair) var) (cons var val) pair)) state))))


(define ifImp
    (lambda (statement state)
            (display "Running ifImp with statement: ") (display statement) (newline)
            (if (M_boolean (cadr statement) state)
                (statementHandler (caddr statement) state)
                (statementHandler (cadddr statement) state))))

(define whileImp
        (lambda (statement state)
                        (display "Running whileImp with statement: ") (display statement) (newline)
                        (if (M_boolean (cadr statement) state)
                            (begin
                                (display "Condition is true, running whileImp with statement: ") (display (caddr statement)) (newline)
                                (let ((new-state (statementHandler (cddr statement) state)))
                                    (whileImp statement new-state))
                            )
                            (begin
                                (display "Condition is false, returning state: ") (display state) (newline)
                                state
                            )
                        )
))

(define M_value
    (lambda (statement state)
            (display "Running M_value with statement: ") (display statement) (newline)
            (display "State: ") (display state) (newline)
            (cond
                [(number? statement)        statement]
                [(symbol? statement)        (lookup state statement)]
                [(eq? (car statement) '+)   (+ (M_value (cadr statement) state) (M_value (caddr statement) state))]
                [(eq? (car statement) '-)   (- (M_value (cadr statement) state) (M_value (caddr statement) state))]
                [(eq? (car statement) '*)   (* (M_value (cadr statement) state) (M_value (caddr statement) state))]
                [(eq? (car statement) '/)   (quotient (M_value (cadr statement) state) (M_value (caddr statement) state))]
                [(eq? (car statement) '%)   (remainder (M_value (cadr statement) state) (M_value (caddr statement) state))]
                [else (lookup state statement)]
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

(define isDeclaration?
    (lambda (statement)
        (eq? (car statement) 'var)))

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
        (display "Running M_boolean with statement: ") (display statement) (newline)
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
(statementHandler (parser "../CSDS345/proj1/test1.txt") '())