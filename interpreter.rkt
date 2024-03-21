#lang racket

; Simple Language Interpreter Project
; Andrej Antunovikj, Daniel Lin, Eric Chen
; CSDS 345 - Programming Language Concepts

(require "simpleParser.rkt") 

(define (statementHandler prog state k)
  (if (null? prog) 
      (k state)
      (statementHandler (cdr prog) (M_state (car prog) state k k k) k)))


; Examples:
; (define pre_op car)
; (define l_operand cadr)
; (define r_operand caddr)

; ; State access abstractions
; (define state_vars car)
; (define state_vals cadr)

; ; Variable access abstractions
; (define var_name cadr)
; (define var_value caddr)

; ; If-statement & while-loop abstractions
; (define condition cadr)
; (define stmt1 caddr)
; (define elif cdddr)
; (define stmt2 cadddr)
; (define loop_body cddr)

; ; Statement list abstractions
; (define curr_stmt car)
; (define next_stmt cdr)
; (define curr_inner_stmt caar)
; (define finally_block cadddr)
; (define catch_block caddr)
; (define catch_var caaddr)
; (define try_block cadr)
; (define throw_block cadr)

; Return abstraction
(define ret_val cadr)

; Empty state
(define empty_state '(()()))

(define (push-state state)
  (cons '() state))

(define (pop-state state)
  (cdr state))

(define (add-binding state key val)
  (cons (cons (cons key val) (car state)) (cdr state)))

(define (remove-binding state key)
  (cons (remove-binding-helper (car state) key) (cdr state)))

(define (remove-binding-helper state key)
  (cond
    [(null? state) null]
    [(eq? (caar state) key) (cdr state)]
    [else (cons (car state) (remove-binding-helper (cdr state) key))]))

(define (lookup state key)
  (cond
    [(null? state) null]
    [(assoc key (car state)) => cdr]
    [else (lookup (cdr state) key)]))


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
    [(null? return-val) (begin (displayln "Returning null") (k null))]
    [(number? return-val) (begin (displayln (format "Returning number: ~a" return-val)) (k return-val))]
    [(symbol? return-val) (begin (displayln (format "Returning symbol: ~a" return-val)) (k (lookup state return-val)))]
    ; If it's a list, it's an expression. Evaluate it.
    [(list? return-val) (M_value return-val state (lambda(v) (k v)))]
    [else (error "Invalid return value" return-val)]))

(define (searchBinding state_key var)
  (cond
    [(null? state_key) (error "Variable is not declared")]
    [(eq? (caar state_key) var) (car state_key)]
    [else (searchBinding (cdr state_key) var)]))

(define (breakImp statement state)
  (displayln "Executing break statement")
  (if (null? (lookup state 'break))
      (error "Break statement outside of loop")
      (add-binding state 'break 'true)))

(define (loop condition body state next break continue)
  (if (M_value condition state (lambda(v) v))
      (with-handlers
          ([(lambda (e) (eq? e 'continue)) (lambda (e) (loop condition body state next break continue))])
        (M_state body state (lambda(s1) (loop condition body s1 next break continue)) break continue))
      (let ((state-without-continue (remove-binding state 'continue))) ; Remove 'continue' binding here
        (next state-without-continue))))

(define (continueImp statement state next)
  (displayln "Executing continue statement")
  (if (null? (lookup state 'continue))
      (error "Continue statement outside of loop")
      (begin
        (add-binding state 'continue 'true)
        (next state))))

(define (tryImp statement state next)
  (displayln "Executing try statement")
  (call/cc
    (lambda (exit)
      (with-handlers
          ([(lambda (e) #t)
            (lambda (e)
              (if (null? (caddr statement))
                  (begin (displayln "Exiting try block") (exit state))
                  (begin (displayln "Executing catch block") (M_state (caddr statement) (add-binding state (cadddr statement) e) (lambda(s1) s1) (lambda(s1) s1)))))])
        (M_state (cadr statement) state (lambda (v) v) (lambda (v) v) (lambda (v) v))
        (if (not (null? (cadddr statement)))
            (M_state (cadddr statement) state (lambda (v) v) (lambda (v) v))
            state)))))

(define (throwImp statement state)
  (displayln "Executing throw statement")
  (if (null? (lookup state 'e))
      (error "Throw statement outside of try block")
      (add-binding state 'e (cadr statement))))

(define (assign statement state)
  (displayln "Executing assignment statement")
  (if (null? (lookup state (cadr statement)))
      (error "Variable is not declared")
      (add-binding state (cadr statement) (M_value (caddr statement) state (lambda(v) v)))
  )
)

(define (declare statement state)
  (displayln "Executing declaration statement")
  (if (not (null? (lookup state (cadr statement))))
      (error "Variable is already declared")
      (if (null? (cddr statement))
          (add-binding state (cadr statement) 'value_undefined)
          (add-binding state (cadr statement) (M_value (caddr statement) state (lambda(v) v))))))

(define (ifImp statement state)
  (displayln "Executing if statement")
  (if (M_value (cadr statement) state (lambda(v) v))
      (M_state (caddr statement) state (lambda(s1) s1) (lambda(s1) s1) (lambda(s1) s1))
      (if (null? (cdddr statement))
          state
          (M_state (cadddr statement) state (lambda(s1) s1) (lambda(s1) s1) (lambda(s1) s1))
      )
  )
)

(define (whileImp statement state next break continue)
  (displayln "Executing while statement")
  (let ((state-with-continue (add-binding state 'continue 'false))) ; Add 'continue' binding here
    (loop (cadr statement) (caddr statement) state-with-continue next (lambda(s1) (next s1)) (lambda(s1) (continue s1)))))

(define (beginImp statement state break)
  (displayln "Executing begin statement")
  (if (null? statement)
      state
      (beginImp (cdr statement) (M_state (car statement) state (cdr statement) break (lambda (v) v)) break)))

(define (catchImp statement state)
  (displayln "Executing catch statement")
  (if (null? (lookup state 'e))
      state
      (add-binding state (cadddr statement) (lookup state 'e))))

(define (M_state statement state next break continue)
  (cond
    [(isBreak? statement) (breakImp statement state next break)]
    [(isContinue? statement) (continueImp statement state next)]
    [(isThrow? statement) (throwImp statement state next)]
    [(isTry? statement) (tryImp statement state next)]
    [(isReturn? statement) (return statement state next)]
    [(isCatch? statement) (catchImp statement state)]
    [(isDeclaration? statement) (declare statement state)]
    [(isAssignment? statement) (assign statement state)]
    [(isIfStatement? statement) (ifImp statement state)]
    [(isWhileStatement? statement) (whileImp statement state next break continue)]
    [(isBeginStatement? statement) (beginImp (cdr statement) state next)]
    [else (error "Invalid statement")]))

(define (M_value statement state k)
  (cond
    [(number? statement) (begin (displayln (format "Returning number: ~a" statement)) (k statement))]
    [(eq? statement 'false) (begin (displayln "Returning boolean: #f") (k #f))]
    [(eq? statement 'true) (begin (displayln "Returning boolean: #t") (k #t))]
    [(eq? (lookup state statement) 'value_undefined) (error "Variable is not assigned a value")]
    [(and (symbol? statement) (null? (lookup state statement))) (error "Variable is not declared")]
    [(symbol? statement) (begin (displayln (format "Returning symbol: ~a" statement)) (k (lookup state statement)))]
    [(and (eq? (car statement) '-) (null? (cddr statement))) (M_value (cadr statement) state (lambda (v) (begin (displayln (format "Returning number: ~a" v)) (k (* v -1)))))]
    [(eq? (car statement) '-) (begin (displayln "Performing subtraction") (k (- (M_value (cadr statement) state (lambda (v) v)) (M_value (caddr statement) state (lambda (v) v)))))]
    [(eq? (car statement) '+) (begin (displayln "Performing addition") (k (+ (M_value (cadr statement) state (lambda (v) v)) (M_value (caddr statement) state (lambda (v) v)))))]
    [(eq? (car statement) '*) (begin (displayln "Performing multiplication") (k (* (M_value (cadr statement) state (lambda (v) v)) (M_value (caddr statement) state (lambda (v) v)))))]
    [(eq? (car statement) '/) (begin (displayln "Performing division") (k (/ (M_value (cadr statement) state (lambda (v) v)) (M_value (caddr statement) state (lambda (v) v)))))]
    [(eq? (car statement) '%) (begin (displayln "Performing modulo") (k (modulo (M_value (cadr statement) state (lambda (v) v)) (M_value (caddr statement) state (lambda (v) v)))))]
    [(eq? (car statement) '>) (begin (displayln "Performing greater than comparison") (k (> (M_value (cadr statement) state (lambda (v) v)) (M_value (caddr statement) state (lambda (v) v)))))]
    [(eq? (car statement) '>=) (begin (displayln "Performing greater than or equal to comparison") (k (>= (M_value (cadr statement) state (lambda (v) v)) (M_value (caddr statement) state (lambda (v) v)))))]
    [(eq? (car statement) '<) (begin (displayln "Performing less than comparison") (k (< (M_value (cadr statement) state (lambda (v) v)) (M_value (caddr statement) state (lambda (v) v)))))]
    [(eq? (car statement) '<=) (begin (displayln "Performing less than or equal to comparison") (k (<= (M_value (cadr statement) state (lambda (v) v)) (M_value (caddr statement) state (lambda (v) v)))))]
    [(eq? (car statement) '!=) (begin (displayln "Performing not equal to comparison") (k (not (eq? (M_value (cadr statement) state (lambda (v) v)) (M_value (caddr statement) state (lambda (v) v))))))]
    [(eq? (car statement) '!) (begin (displayln "Performing logical negation") (k (not (M_value (cadr statement) state (lambda (v) v)))))]
    [(eq? (car statement) '&&) (begin (displayln "Performing logical AND") (k (and (M_value (cadr statement) state (lambda (v) v)) (M_value (caddr statement) state (lambda (v) v)))))]
    [(eq? (car statement) '||) (begin (displayln "Performing logical OR") (k (or (M_value (cadr statement) state (lambda (v) v)) (M_value (caddr statement) state (lambda (v) v)))))]
    [(eq? (car statement) '==) (begin (displayln "Performing equality comparison") (k (eq? (M_value (cadr statement) state (lambda (v) v)) (M_value (caddr statement) state (lambda (v) v)))))]
    [else (error "Invalid value")]))

(define (execute filename)
  (statementHandler (parser filename) '(()()) (lambda (s) s)))

(parser "test.java") ; returns the parsed list of statements, for debugging purposes
(statementHandler (parser "test.java") '(()()) (lambda (s) s))
