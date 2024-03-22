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

(define (isReturn? statement)
        (eq? (operator statement) 'return))

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

(define (isTry? statement)
        (eq? (operator statement) 'try))

(define (isThrow? statement)
        (eq? (operator statement) 'throw))

(define (statementHandler prog state)
  (call/cc (lambda(return)
  (call/cc (lambda(throw)
        (statementHandler (rest-of-prog prog) (M_state (curr-stmt prog) state (lambda(v) v) (lambda(s) s) (lambda(c) c) return throw))
            )))))

; an example of state is '((x 10) (y 9)). caar access 'x, and cadar access '10.
(define (lookup state key)
            (cond
                [(null? state)                          null]
                [(eq? (var-name state) key)             (var-value state)]
                [else                                   (lookup (cdr state) key)]
            ))

(define (removeBinding state var)
    (cond
        [(null? (lookup state var))                (error "Variable is not declared")]
        [(eq? (var-name state) var)                (cdr state)]
        [else                                      (cons (car state) (removeBinding (cdr state) var))]
))

(define (try-catch-finally statement state next break continue throw)
    (cond
        [(and (null? (cadddr statement)) (null? (caddr statement)))     ; both finally blk and catch blk doesn't exist
            (M_state (cadr statement) state next break continue throw)] ; throw exception out, if has, and no finally blk to be done
        [(null? (caddr statement))                                      ; catch blk doesn't exist
            (M_state (cadr statement) state 
                (lambda(s) (M_state (cadddr statement) s next break continue throw))
                    next break continue throw)]                         ; execute finally blk after
        [(null? (cadddr statement))                                     ; finally blk doesn't exist
            (M_state (cadr statement) state next break continue 
                (lambda(s) (M_state (caddr statement) s next break continue throw)))]
        [else (M_state (cadr statement) state                           ; both blks exist
            (lambda(s) (M_state (cadddr statement) s next break continue throw))
                break continue 
                    (lambda(v) (M_state(caddr statement) v next break continue throw)))]
))

(define (addBinding state var value)
    (append state (list (list var value))))

(define (assign statement state next break continue throw)
    (addBinding (removeBinding state (defined-var statement)) (defined-var statement) (M_value (caddr statement) state)))

(define (declare statement state next break continue throw)
            (if (not (null? (lookup state (defined-var statement))))
                (error "Variable is already declared")
                ;whether the variable is given a initial value
                (if (null? (defined-var-value statement))
                    (addBinding state (cadr statement) 'value_undefined)
                    (addBinding state (cadr statement) (M_value (caddr statement) state))
                )))

(define (ifImp statement state next break continue return throw)
            (if (M_value (condition statement) state)
                (M_state (body statement) state next break continue return throw)
                ;whether the third argument 'else' exist
                (if (null? (cdddr statement)) 
                    state
                    (M_state (cadddr statement) state next break continue return throw))))

(define (beginImp statement state next break continue return throw)
  ;local variable added by using addLocalLayer()
  (M_state (car statement) (addLocalLayer state) (lambda(s) (M_state (cadr statement) s next break continue return)) break continue return)) 

(define (addLocalLayer state)
  (list state '()))

(define (whileImp statement state next break continue return throw)
    (loop (defined-var statement) (defined-var-value statement) state next
          (lambda(s1) (next s1))
          (lambda(s2) (loop (condition statement) (body statement) s2 next break continue return throw)) return throw))

;[Daniel]: (cadr statement) is condition, (caddr statement) is body
(define (loop condition body state next break continue return throw)
    (if (M_value condition state)
        (M_state body state (lambda(s1) (loop condition body s1 next break continue return throw)) break continue return throw)
        (next state)))

(define (M_state statement state next break continue return throw)
            (cond
                [(isBreak? statement)           (break state)]
                [(isContinue? statement)        (continue state)]
                [(isThrow? statement)           (throw state)]
                [(isBegin? statement)           (beginImp (cdr statement) state next break continue return throw)] ;passing everything except 'begin
                [(isReturn? statement)          (return (lookup state (cadr statement)))]
                [(isDeclaration? statement)     (next (declare statement state next break continue throw))]
                [(isAssignment? statement)      (next (assign statement state next break continue throw))]
                [(isIfStatement? statement)     (next (ifImp statement state next break continue return throw))]
                [(isWhileStatement? statement)  (next (whileImp statement state next break continue return throw))]
                [(isTry? statement)             (next (try-catch-finally statement state next break continue return throw))]
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