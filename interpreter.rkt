#lang racket

; Simple Language Interpreter Project
; Andrej Antunovikj, Daniel Lin, Eric Chen
; CSDS 345 - Programming Language Concepts

(require "simpleParser.rkt")

; Abstractions for car, cdr, etc.
(define (curr-stmt prog) (car prog))
(define (rest-of-prog prog) (cdr prog))
(define (var-name state) (caar state))
(define (var-value state) (cadar state))
(define (operator stmt) (car stmt))
(define (defined-var stmt) (cadr stmt))
(define (defined-var-value stmt) (caddr stmt))
(define (condition stmt) (cadr stmt))
(define (body stmt) (caddr stmt))

(define (statementHandler prog state)
  (call/cc
   (lambda(return)
        (cond   
            [(not (null? (lookup state 'return))) (lookup state 'return)]
            [else (statementHandler (rest-of-prog prog) (M_state (curr-stmt prog) state (lambda(v) v) (lambda(s) s) (lambda(c) c) return))]
            ))))

; an example of state is '((x 10) (y 9)). caar access 'x, and cadar access '10.
(define (lookup state key)
            (cond
                [(null? state)                          null]
                [(eq? (var-name state) key)                 (var-value state)]
                [else                                   (lookup (cdr state) key)]
            ))

(define (isReturn? statement)
        (eq? (operator statement) 'return))

;[Daniel]: In my view, the return function doesn't 'return' the value. Instead, it store the value into state 
; and StatementHandler could return it in the next recurively call.  
(define (isDeclaration? statement)
        (eq? (operator statement) 'var))

(define (isAssignment? statement)
        (eq? (operator statement) '=))

(define (isBreak? statement)
        (eq? (operator statement) 'break))

(define (isBegin? statement)
        (eq? (operator statement) 'begin))

(define (isIfStatement? statement)
        (and (list? statement) (eq? (operator statement) 'if)))

(define (isWhileStatement? statement)
        (and (list? statement) (eq? (operator statement) 'while)))

(define (isContinue? statement)
        (eq? (operator statement) 'continue))

(define (returnImp statement state next break continue)
    (cond
        [(number? (M_value (defined-var statement) state)) (append state (list (list 'return (M_value (defined-var statement) state))))]
        [else 
            (if (M_value (defined-var statement) state)
                (append state (list (list 'return 'true)))
                (append state (list (list 'return 'false))))]))

(define (removeBinding state var)
    (cond
        [(null? (lookup state var))                (error "Variable is not declared")]
        [(eq? (var-name state) var)                    (cdr state)]
        [else                                      (cons (car state) (removeBinding (cdr state) var))]
))

(define (addBinding state var value)
    (append state (list (list var value))))

(define (assign statement state next break continue)
    (addBinding (removeBinding state (defined-var statement)) (defined-var statement) (M_value (caddr statement) state)))

(define (declare statement state next break continue)
            (if (not (null? (lookup state (defined-var statement))))
                (error "Variable is already declared")
                ;whether the variable is given a initial value
                (if (null? (defined-var-value statement))
                    (addBinding state (cadr statement) 'value_undefined)
                    (addBinding state (cadr statement) (M_value (caddr statement) state))
                )))

; [Daniel]: We probably doesn't need this. Use call/cc instead
(define (ifImp statement state next break continue return)
            (if (M_value (condition statement) state)
                (M_state (body statement) state next break continue return)
                ;whether the third argument 'else' exist
                (if (null? (cdddr statement)) 
                    state
                    (M_state (cadddr statement) state next break continue return))))

(define (beginImp statement state next break continue return)
  ;local variable
  (M_state (car statement) state (lambda(s) (M_state (cadr statement) s next break continue return)) break continue return)) 


(define (whileImp statement state next break continue return)
    (loop (defined-var statement) (defined-var-value statement) state next
          (lambda(s1) (next s1))
          (lambda(s2) (loop (condition statement) (body statement) s2 next break continue return)) return))

;[Daniel]: (cadr statement) is condition, (caddr statement) is body
(define (loop condition body state next break continue return)
    (if (M_value condition state)
        (M_state body state (lambda(s1) (loop condition body s1 next break continue return)) break continue return)
        (next state)))

(define (M_state statement state next break continue return)
            (cond
                [(isBreak? statement)           (break state)]
                [(isContinue? statement)        (continue state)]
                [(isBegin? statement)           (beginImp (cdr statement) state next break continue return)] ;passing everything except 'begin
                [(isReturn? statement)          (return (lookup state (cadr statement)))]
                [(isDeclaration? statement)     (next (declare statement state next break continue))]
                [(isAssignment? statement)      (next (assign statement state next break continue))]
                [(isIfStatement? statement)     (next (ifImp statement state next break continue return))]
                [(isWhileStatement? statement)  (next (whileImp statement state next break continue return))]
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
            [(and (eq? (operator statement) '-) (null? (cddr statement)))    (* (M_value (cadr statement) state) -1)]

            [(eq? (operator statement) '+)   (+ (M_value (defined-var statement) state) (M_value (caddr statement) state))]  
            [(eq? (operator statement) '-)   (- (M_value (defined-var statement) state) (M_value (caddr statement) state))]
            [(eq? (operator statement) '*)   (* (M_value (defined-var statement) state) (M_value (caddr statement) state))]
            [(eq? (operator statement) '/)   (quotient (M_value (defined-var statement) state) (M_value (caddr statement) state))]
            [(eq? (operator statement) '%)   (remainder (M_value (defined-var statement) state) (M_value (caddr statement) state))]
            [(eq? (operator statement) '>)   (> (M_value (defined-var statement) state) (M_value (caddr statement) state))]
            [(eq? (operator statement) '>=)  (>= (M_value (defined-var statement) state) (M_value (caddr statement) state))]
            [(eq? (operator statement) '<)   (< (M_value (defined-var statement) state) (M_value (caddr statement) state))]
            [(eq? (operator statement) '<=)  (<= (M_value (defined-var statement) state) (M_value (caddr statement) state))]
            [(eq? (operator statement) '!=)  (not (= (M_value (defined-var statement) state) (M_value (caddr statement) state)))]
            [(eq? (operator statement) '!)   (not (M_value (defined-var statement) state))]
            [(eq? (operator statement) '&&)  (and (M_value (defined-var statement) state) (M_value (caddr statement) state))]
            [(eq? (operator statement) '||)  (or (M_value (defined-var statement) state) (M_value (caddr statement) state))]
            [(eq? (operator statement) '==)  (eq? (M_value (defined-var statement) state) (M_value (caddr statement) state))]
            [else (error "not a vaild value")]
        ))

(parser "test.java") ; returns the parsed list of statements, for debugging purposes
(statementHandler (parser "test.java") '()) ; returns the final state, for debugging purposes