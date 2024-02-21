#lang racket

; Simple Language Interpreter Project
; Andrej Antunovikj, Daniel Lin, Eric Chen
; CSDS 345 - Programming Language Concepts

(require "simpleParser.rkt") 

; Simple Language Interpreter Project
; Andrej Antunovikj, Daniel Lin, Eric Chen
; CSDS 345 - Programming Language Concepts

;TODO: lookup is called too many times. 

(define (statementHandler prog state)
        (cond   
            [(not (null? (lookup state 'return))) (lookup state 'return)]
            [else (statementHandler (cdr prog) (M_state (car prog) state))]
            ))

; an example of state is '((x 10) (y 9)). caar access 'x, and cadar access '10.
(define (lookup state key)
            (cond
                [(null? state)                          null]
                [(eq? (caar state) key)                 (cadar state)]
                [else                                   (lookup (cdr state) key)]
            ))

(define (isReturn? statement)
        (eq? (car statement) 'return))

;[Daniel]: In my view, the return function doesn't 'return' the value. Instead, it store the value into state 
; and StatementHandler could return it in the next recurively call.  
(define (isDeclaration? statement)
        (eq? (car statement) 'var))

(define (isAssignment? statement)
        (eq? (car statement) '=))

(define (isIfStatement? statement)
        (and (list? statement) (eq? (car statement) 'if)))

(define (isWhileStatement? statement)
        (and (list? statement) (eq? (car statement) 'while)))

(define (return statement state)
    (cond
        [(number? (M_value (cadr statement) state)) (append state (list (list 'return (M_value (cadr statement) state))))]
        [else 
            (if (M_value (cadr statement) state)
                (append state (list (list 'return 'true)))
                (append state (list (list 'return 'false))))]))

(define (removeBinding state var)
    (cond
        [(null? (lookup state var))                (error "Variable is not declared")]
        [(eq? (caar state) var)                    (cdr state)]
        [else                                      (cons (car state) (removeBinding (cdr state) var))]
))

(define (addBinding state var value)
    (append state (list(list var value))))

(define (assign statement state)
    (addBinding (removeBinding state (cadr statement)) (cadr statement) (M_value (caddr statement) state)))

(define (declare statement state)
            (if (not (null? (lookup state (cadr statement))))
                (error "Variable is already declared")
                ;whether the variable is given a initial value
                (if (null? (cddr statement))
                    (addBinding state (cadr statement) 'value_undefined)
                    (addBinding state (cadr statement) (M_value (caddr statement) state))
                )))

(define (ifImp statement state)
            (if (M_value (cadr statement) state)
                (M_state (caddr statement) state)
                ;whether the third argument 'else' exist
                (if (null? (cdddr statement)) 
                    state
                    (M_state (cadddr statement) state))))

(define (whileImp statement state)
        (if (M_value (cadr statement) state)
            (M_state statement (M_state (caddr statement) state))
            state))

(define (M_state statement state)
            (cond
                [(isReturn? statement)          (return statement state)]
                [(isDeclaration? statement)     (declare statement state)]
                [(isAssignment? statement)      (assign statement state)]
                [(isIfStatement? statement)     (ifImp statement state)]
                [(isWhileStatement? statement)  (whileImp statement state)]
                [else (error "Invalid statement")]
            ))

(define (M_value statement state)
        (cond
            [(number? statement)                                        statement]
            [(eq? statement 'false)                                     #f]
            [(eq? statement 'true)                                      #t]

            ;error check: when a variale is not assigned a value
            [(eq? (lookup state statement) 'value_undefined)            (error "Variable is not assigned a value")]

            ;error check: when a variable is not declared (it is a symbol and not appeared in state)
            [(and (symbol? statement) (null? (lookup state statement))) (error "Variable is not declared")]

            [(symbol? statement)                                        (lookup state statement)]

            ;special condition when '- acts as negative sign
            [(and (eq? (car statement) '-) (null? (cddr statement)))    (* (M_value (cadr statement) state) -1)]

            [(eq? (car statement) '+)   (+ (M_value (cadr statement) state) (M_value (caddr statement) state))]  
            [(eq? (car statement) '-)   (- (M_value (cadr statement) state) (M_value (caddr statement) state))]
            [(eq? (car statement) '*)   (* (M_value (cadr statement) state) (M_value (caddr statement) state))]
            [(eq? (car statement) '/)   (quotient (M_value (cadr statement) state) (M_value (caddr statement) state))]
            [(eq? (car statement) '%)   (remainder (M_value (cadr statement) state) (M_value (caddr statement) state))]
            [(eq? (car statement) '>)   (> (M_value (cadr statement) state) (M_value (caddr statement) state))]
            [(eq? (car statement) '>=)  (>= (M_value (cadr statement) state) (M_value (caddr statement) state))]
            [(eq? (car statement) '<)   (< (M_value (cadr statement) state) (M_value (caddr statement) state))]
            [(eq? (car statement) '<=)  (<= (M_value (cadr statement) state) (M_value (caddr statement) state))]
            [(eq? (car statement) '!=)  (not (= (M_value (cadr statement) state) (M_value (caddr statement) state)))]
            [(eq? (car statement) '!)   (not (M_value (cadr statement) state))]
            [(eq? (car statement) '&&)  (and (M_value (cadr statement) state) (M_value (caddr statement) state))]
            [(eq? (car statement) '||)  (or (M_value (cadr statement) state) (M_value (caddr statement) state))]
            [(eq? (car statement) '==)  (eq? (M_value (cadr statement) state) (M_value (caddr statement) state))]
            [else (error "not a vaild value")]
        ))

(define (execute filename)
    (statementHandler (parser "test.java") '()))

(statementHandler (parser "test.java") '())