#lang racket

(define statementHandler
    (lambda (prog state)
        (cond   
            [(not (null? (lookup state 'return))) (lookup state 'return)]
            [else (statementHandler (cdr prog) (M_state (car prog) state))]
            )))

(define lookup
    (lambda (state key)
            (display "Looking up key: ") (display key) (newline)
            (cond
                [(null? state) null]
                [(eq? (caar state) key) (cadar state)]
                [else (lookup (cdr state) key)]
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

(define (return statement state)
            (append state (list (list 'return (M_value (cadr statement) state)))))

(define (removeBinding state var)
    (cond
        [(null? (lookup state var)) (error "Variable is not declared")]
        [(eq? (caar state) var) (cdr state)]
        [else (list (car state) (removeBinding (cdr state) var))] 
    ))

(define (addBinding state var value)
    (append state (list(list var value))))

(define (assign statement state)
        (addBinding (removeBinding state (cadr statement)) (cadr statement) (caddr statement)))

(define declare
    (lambda (statement state)
            (if (not (null? (lookup state (cadr statement))))
                (error "Variable already declared")
                (if (null? (cddr statement))
                    (addBinding state (cadr statement) 0)
                    (addBinding state (cadr statement) (M_value (caddr statement) state))
                ))))

(define ifImp
    (lambda (statement state)
            (display "Running ifImp with statement: ") (display statement) (newline)
            (if (M_boolean (cadr statement) state)
                (M_state (caddr statement) state)
                (M_state (cadddr statement) state))))

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




;TODO: we shouldn't return #t or #f but true or false here.
(define M_boolean 
    (lambda (statement state)
        (display "Running M_boolean with statement: ") (display statement) (newline)
        (cond
            [(eq? (car statement) '>)  (> (M_value (cadr statement) state) (M_value (caddr statement) state))]
            [(eq? (car statement) '>=) (>= (M_value (cadr statement) state) (M_value (caddr statement) state))]
            [(eq? (car statement) '<)  (< (M_value (cadr statement) state) (M_value (caddr statement) state))]
            [(eq? (car statement) '<=) (<= (M_value (cadr statement) state) (M_value (caddr statement) state))]
            [(eq? (car statement) '!=) (not (= (M_value (cadr statement) state) (M_value (caddr statement) state)))]
            [(eq? (car statement) '&&) (and (M_boolean (cadr statement) state) (M_boolean (caddr statement) state))]
            [(eq? (car statement) '||) (or (M_boolean (cadr statement) state) (M_boolean (caddr statement) state))]
            [else (error "not a boolean")]
        )))