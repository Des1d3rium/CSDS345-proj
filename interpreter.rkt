#lang racket

; Simple Language Interpreter Project
; Andrej Antunovikj, Daniel Lin, Eric Chen
; CSDS 345 - Programming Language Concepts

(require "simpleParser.rkt") 

;TODO: lookup is called too many times. 

; [Andrej]: The statement handler now has a with-handlers block that handles
; raised return values. If the statement handler encounters a return
; block, the return block will raise the value and the with-handlers block will catch the
; value and return it.

(define (statementHandler prog state)
    (with-handlers
        ([(lambda (e) #t)
         (lambda (e)
           (if (eq? e 'true) #t
               (if (eq? e 'false) #f
                   e)))])
      (if (null? prog) 
          state
          (statementHandler (cdr prog) (M_state (car prog) state)))))

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
    (and (list? statement) (eq? (car statement) 'begin)))

;[Daniel]: In my view, the return function doesn't 'return' the value. Instead, it store the value into state 
; and StatementHandler could return it in the next recurively call.  
;[Andrej]: In accordance with this, I have updated the return function to raise the value
; instead of returning it. This way, the statement handler can catch the value and return it.
; See statementHandler and return for more details.
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
    ; [Andrej]: The return function now raises the value instead of returning it. The
    ; statement handler will catch the value and return it.
        [(number? (M_value (cadr statement) state)) (raise (M_value (cadr statement) state))]
        ; If it's not an integer, it's a boolean value
        [else 
            (if (M_value (cadr statement) state)
                (raise 'true)
                (raise 'false)
                )]))

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

(define (tryImp statement state)
  (call-with-current-continuation
   (lambda (exit)
     (with-handlers
         ([(lambda (e) #t)
           (lambda (e)
             (if (null? (caddr statement))
                 (exit state)
                 (M_state (caddr statement) (addBinding state 'e e 'front))))])
       (M_state (cadr statement) state)
       (if (not (null? (cadddr statement)))
              (M_state (cadddr statement) state)
              state)))))

(define (throwImp statement state)
  (raise (M_value (cadr statement) state)))

(define (assign statement state)
    (if (null? (lookup state (cadr statement)))                
        (error "Variable is not declared")
        (addBinding state (cadr statement) (M_value (caddr statement) state) (searchBinding (car state) (cadr statement)))))

(define (declare statement state)
            (if (not (null? (lookup state (cadr statement))))
                (error "Variable is already declared")
                ;whether the variable is given a initial value
                (if (null? (cddr statement))
                    (addBinding state (cadr statement) 'value_undefined 'front)
                    (addBinding state (cadr statement) (M_value (caddr statement) state) 'front))))

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

(define (beginImp statement state)
    (if (null? statement)
        state
        (beginImp (cdr statement) (M_state (car statement) state))))

(define (catchImp statement state)
    (with-handlers
            ([(lambda (e) #t)
                (lambda (e)
                    (M_state (caddr statement) (addBinding state 'e e 'front)))])
        (M_state (cadr statement) state)))

(define (M_state statement state)
            (cond
                [(isBreak? statement) (breakImp statement state)]
                [(isContinue? statement) (continueImp statement state)]
                [(isThrow? statement) (throwImp statement state)]
                [(isTry? statement) (tryImp statement state)]
                [(isReturn? statement)          (return statement state)]
                [(isCatch? statement) (catchImp statement state)]
                [(isDeclaration? statement)     (declare statement state)]
                [(isAssignment? statement)      (assign statement state)]
                [(isIfStatement? statement)     (ifImp statement state)]
                [(isWhileStatement? statement)  (whileImp statement state)]
                [(isBeginStatement? statement)  (beginImp (cdr statement) state)]
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
    (statementHandler (parser filename) '(()())))

(parser "test.java") ; returns the parsed list of statements, for debugging purposes
(statementHandler (parser "test.java") '(()()))