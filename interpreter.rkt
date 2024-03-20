#lang racket

; Simple Language Interpreter Project
; Andrej Antunovikj, Daniel Lin, Eric Chen
; CSDS 345 - Programming Language Concepts

(require "simpleParser.rkt") 

(define (statementHandler prog state k)
  (if (null? prog) 
      (k state)
      (statementHandler (cdr prog) (M_state (car prog) state k k) k)))

; TODO: lookup is called too many times. 
; an example of state is '((x 10) (y 9)). caar access 'x, and cadar access '10.
; [Daniel]: change to '((x y) (10 9)). (car state) access key, and (cadr state) access value.
; TODO: working on 'list of layers' that support local variables.  
(define (lookup state key)
    (if(null? state) 
        null
        (lookup-rec (car state) (cadr state) key)))

(define (lookup-rec state-key state-val key)
    (cond 
        [(null? state-key)              null]
        [(eq? (car state-key) key)      (car state-val)]
        [else                           (lookup-rec (cdr state-key) (cdr state-val) key)]
    ))

(define (isReturn? statement)
        (eq? (car statement) 'return))

(define (isBreak? statement)
        (eq? (car statement) 'break))
    
(define (isContinue? statement)
        (eq? (car statement) 'continue))

(define (isThrow? statement)
        (eq? (car statement) 'throw))

(define (isTry? statement)
        (eq? (car statement) 'try))

(define (isCatch? statement)
        (eq? (car statement) 'catch))

(define (isBeginStatement? statement)
        (eq? (car statement) 'begin))

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

(define (return statement state k)
    (define return-val (cadr statement))
        (cond
            [(null? return-val) (k null)]
            [(number? return-val) (k return-val)]
            [(symbol? return-val) (k (lookup state return-val))]
            [else (error "Invalid return value")]))

; [Daniel]: now it return the index of key. This is passed as a parameter in addBinding to remove and re-add binding.
(define (searchBinding state_key var)
    (cond
        [(null? state_key)                 (error "Key not found in removing binding.")]
        [(eq? (car state_key) var)         0]
        [else                              (+ 1 (searchBinding (cdr state_key) var))]))

(define (addBinding state key val pos)
    (list (addBinding-rec (car state) key pos) (addBinding-rec (cadr state) val pos)))

(define (addBinding-rec state value k)
    (cond
        [(eq? k 'front)                         (cons value state)]
        [(and (eq? k 0) (null? state))          (cons value '())]
        [(eq? k 0)                              (cons value (cdr state))]
        [else                                   (cons (car state) (addBinding-rec (cdr state) value (- k 1)))]
    ))

(define (breakImp statement state)
  ; Implementation of break statement
    (if (null? (lookup state 'break))
        (error "Break statement outside of loop")
        (addBinding state 'break 'true 'front)
    )
  )

(define (continueImp statement state)
    (if (null? (lookup state 'continue))
        (error "Continue statement outside of loop")
        (addBinding state 'continue 'true 'front)
    )
  ; Implementation of continue statement
  )

(define (tryImp statement state next)
  (call/cc
   (lambda (exit)
     (with-handlers
         ([(lambda (e) #t)
           (lambda (e)
             (if (null? (caddr statement))
                 (exit state)
                 (M_state (caddr statement) (addBinding state 'e e 'front) (lambda (v) v) (lambda (v) v))))])
       (M_state (cadr statement) state (lambda (v) v) (lambda (v) v))
       (if (not (null? (cadddr statement)))
              (M_state (cadddr statement) state (lambda (v) v) (lambda (v) v))
              state)))))

(define (throwImp statement state)
    (if (null? (lookup state 'e))
        (error "Throw statement outside of try block")
        (addBinding state 'e (lookup state 'e) 'front)
    )
)

(define (assign statement state)
    (if (null? (lookup state (cadr statement)))                
        (error "Variable is not declared")
        (addBinding state (cadr statement) (M_value (caddr statement) state (lambda(v) v))
            (searchBinding (car state) (cadr statement)))
    )
)

(define (declare statement state)
            (if (not (null? (lookup state (cadr statement))))
                (error "Variable is already declared")
                ;whether the variable is given a initial value
                (if (null? (cddr statement))
                    (addBinding state (cadr statement) 'value_undefined 'front)
                    (addBinding state (cadr statement) (M_value (caddr statement) state (lambda(v) v)) 'front))))

(define (ifImp statement state)
            (if (M_value (cadr statement) state (lambda(v) v))
                (M_state (caddr statement) state (lambda(s1) s1) (lambda(s1) s1))
                ;whether the third argument 'else' exist
                (if (null? (cdddr statement)) 
                    state
                    (M_state (cadddr statement) state (lambda(s1) s1) (lambda(s1) s1))
                )
            )
)

(define (whileImp statement state next break)
    (loop (cadr statement) (caddr statement) state next (lambda(s1) (next s1))))

;[Daniel]: (cadr statement) is condition, (caddr statement) is body
(define (loop condition body state next break)
    (if (M_value condition state (lambda(v) v))
        (M_state body state (lambda(s1) (loop condition body s1 next break)) break)
        (next state)))

(define (beginImp statement state break)
    (if (null? statement)
        state
        (beginImp (cdr statement) (M_state (car statement) state (cdr statement) break) break)))

(define (catchImp statement state)
    (if (null? (lookup state 'e))
        state
        (addBinding state (cadr statement) (lookup state 'e) (searchBinding (car state) (cadr statement))))
)

(define (M_state statement state next break)
  (cond
    [(isBreak? statement)           (breakImp statement state next break)]
    [(isContinue? statement)        (continueImp statement state next)]
    [(isThrow? statement)           (throwImp statement state next)]
    [(isTry? statement)             (tryImp statement state next)]
    [(isReturn? statement)          (return statement state next)]
    [(isCatch? statement)           (catchImp statement state)]
    [(isDeclaration? statement)     (declare statement state)]
    [(isAssignment? statement)      (assign statement state)]
    [(isIfStatement? statement)     (ifImp statement state)]
    [(isWhileStatement? statement)  (whileImp statement state next break)]
    [(isBeginStatement? statement)  (beginImp (cdr statement) state next)]
    [else (error "Invalid statement")]
  ))

(define (M_value statement state k)
  (cond
    [(number? statement)                                        (k statement)]
    [(eq? statement 'false)                                     (k #f)]
    [(eq? statement 'true)                                      (k #t)]
    [(eq? (lookup state statement) 'value_undefined)            (error "Variable is not assigned a value")]
    [(and (symbol? statement) (null? (lookup state statement))) (error "Variable is not declared")]
    [(symbol? statement)                                        (k (lookup state statement))]
    ;special condition when '- acts as negative sign
    [(and (eq? (car statement) '-) (null? (cddr statement)))    (M_value (cadr statement) state (lambda (v) (k (* v -1))))]
    [(eq? (car statement) '-)                                  (k (- (M_value (cadr statement) state (lambda (v) v)) (M_value (caddr statement) state (lambda (v) v))))]
    [(eq? (car statement) '+)                                   (k (+ (M_value (cadr statement) state (lambda (v) v)) (M_value (caddr statement) state (lambda (v) v))))]
    [(eq? (car statement) '*)                                   (k (* (M_value (cadr statement) state (lambda (v) v)) (M_value (caddr statement) state (lambda (v) v))))]
    [(eq? (car statement) '/)                                   (k (/ (M_value (cadr statement) state (lambda (v) v)) (M_value (caddr statement) state (lambda (v) v))))]
    [(eq? (car statement) '%)                                   (k (modulo (M_value (cadr statement) state (lambda (v) v)) (M_value (caddr statement) state (lambda (v) v))))]
    [(eq? (car statement) '>)                                   (k (> (M_value (cadr statement) state (lambda (v) v)) (M_value (caddr statement) state (lambda (v) v))))]
    [(eq? (car statement) '>=)                                  (k (>= (M_value (cadr statement) state (lambda (v) v)) (M_value (caddr statement) state (lambda (v) v))))]
    [(eq? (car statement) '<)                                   (k (< (M_value (cadr statement) state (lambda (v) v)) (M_value (caddr statement) state (lambda (v) v))))]
    [(eq? (car statement) '<=)                                  (k (<= (M_value (cadr statement) state (lambda (v) v)) (M_value (caddr statement) state (lambda (v) v))))]
    [(eq? (car statement) '!=)                                  (k (not (eq? (M_value (cadr statement) state (lambda (v) v)) (M_value (caddr statement) state (lambda (v) v)))))]
    [(eq? (car statement) '!)                                   (k (not (M_value (cadr statement) state (lambda (v) v ))))]
    [(eq? (car statement) '&&)                                  (k (and (M_value (cadr statement) state (lambda (v) v)) (M_value (caddr statement) state (lambda (v) v))))]
    [(eq? (car statement) '||)                                  (k (or (M_value (cadr statement) state (lambda (v) v)) (M_value (caddr statement) state (lambda (v) v))))]
    [(eq? (car statement) '==)                                  (k (eq? (M_value (cadr statement) state (lambda (v) v)) (M_value (caddr statement) state (lambda (v) v))))]
    [else (error "Invalid value")]
))

(define (execute filename)
  (statementHandler (parser filename) '(()()) (lambda (s) s)))

(parser "test.java") ; returns the parsed list of statements, for debugging purposes
(statementHandler (parser "test.java") '(()()) (lambda (s) s)) ; returns the final state, for debugging purposes