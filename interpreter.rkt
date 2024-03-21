#lang racket

; Simple Language Interpreter Project
; Andrej Antunovikj, Daniel Lin, Eric Chen
; CSDS 345 - Programming Language Concepts

(require "simpleParser.rkt") 

; Evaluation abstractions
(define pre_op car)
(define l_operand cadr)
(define r_operand caddr)

; State access abstractions
(define state_vars car)
(define state_vals cadr)

; Variable access abstractions
(define var_name cadr)
(define var_value caddr)

; If-statement & while-loop abstractions
(define condition cadr)
(define stmt1 caddr)
(define elif cdddr)
(define stmt2 cadddr)
(define loop_body cddr)

; Statement list abstractions
(define curr_stmt car)
(define next_stmt cdr)
(define curr_inner_stmt caar)
(define finally_block cadddr)
(define catch_block caddr)
(define catch_var caaddr)
(define try_block cadr)
(define throw_block cadr)

; Return abstraction
(define ret_val cadr)

; Empty state
(define empty_state '(()()))

; Checks if an atom is a potential variable name.
(define var?
  (lambda (x)
    (not (or (pair? x) (null? x)))))

; Retrieves the value of a given variable.
; Here, the state takes the form ((var1 var2 var3 ...) (val1 val2 val3 ...) [nested states here]).
(define find_var
  (lambda (var state)
    (cond
      [(equal? state empty_state) (error 'error_var_not_declared "Variable not declared: ~a" var)]
      [(null? (state_vars state)) (find_var var (pop_inner_state state))]
      [(and (eq? var (car (state_vars state))) (void? (unbox (car (state_vals state))))) (error 'error_var_not_assigned "Variable not assigned: ~a" var)]
      [(eq? var (car (state_vars state))) (unbox (car (state_vals state)))]
      [else (find_var var (cons (cdr (state_vars state)) (cons (cdr (state_vals state)) (cddr state))))])))

; Adds a variable to the state and returns the state.
; If the variable already exists in the state, then raise an error.
(define add_var
  (lambda (var val state)
    (cond
      [(declared? var state) (error 'error_var_doubledeclare "Variable already declared: ~a" var)]
      [(null? (cddr state)) (list (cons var (state_vars state)) (cons (box val) (state_vals state)))]
      [else (list (cons var (state_vars state)) (cons (box val) (state_vals state)) (pop_inner_state state))])))

(define declared?
  (lambda (var state)
    (cond
      [(equal? state empty_state) #f]
      [(null? (state_vars state)) #f]
      [(eq? var (car (state_vars state))) #t]
      [else (declared? var (cons (cdr (state_vars state)) (cons (cdr (state_vals state)) (cddr state))))])))

; Assigns a particular value to a given variable.
; This utilizes set-box!, which will cause side effects by default.
(define assign_var!
  (lambda (var value state)
    (call/cc (lambda (end) (assign_var_helper! var value state state)))))

(define assign_var_helper!
  (lambda (var value state end)
    (cond
      [(equal? state empty_state) (error 'error_var_not_declared "Variable not declared: ~a" var)]
      [(null? (state_vars state)) (assign_var_helper! var value (pop_inner_state state) end)]
      [(eq? var (car (state_vars state))) (begin (set-box! (car (state_vals state)) value) end)]
      [else (assign_var_helper! var value (cons (cdr (state_vars state)) (cons (cdr (state_vals state)) (cddr state))) end)])))


(define (statementHandler prog state k)
  (if (null? prog) 
      (k state)
      (statementHandler (cdr prog) (M_state (car prog) state k k) k)))

; TODO: lookup is called too many times. 
; an example of state is '((x 10) (y 9)). caar access 'x, and cadar access '10.
; [Daniel]: change to '((x y) (10 9)). (car state) access key, and (cadr state) access value.
; scopes is now a list of lists, where each list is a scope.
(define (lookup state var)
; If the "variable" is actually a statement, then we need to evaluate it.
  (if (and (list? var) (not (symbol? var)))
      (M_value var state (lambda (v) v))
      ; Otherwise, we can just look it up.
      (find_var var state))
)

; Creates a new layer for the state.
(define create_inner_state
  (lambda (state)
    (cons '() '() state)))

; Pops the newest layer from the state.
(define pop_inner_state
  (lambda (state)
    (if (null? (cddr state))
        (error 'error_illegal_break "Cannot break here.")
        (caddr state))))

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
(define (searchBinding scopes var)
    (if (null? scopes)
            (error "Key not found in removing binding.")
            (let ((index (findIndex (car scopes) var)))
                (if index
                        index
                        (searchBinding (cdr scopes) var)))))

(define (findIndex scope var)
    (let loop ((scope scope) (index 0))
        (cond
            [(null? scope) null]
            [(eq? (caar scope) var) index]
            [else (loop (cdr scope) (+ index 1))])))

(define (addBinding scopes key val)
  (add_var key val scopes))

(define (updateBinding scopes key val index)
  (assign_var! key val scopes))

(define (updateScope scope key val index)
    (if (eq? index 0)
            (cons (cons key val) (cdr scope))
            (cons (car scope) (updateScope (cdr scope) key val (- index 1)))))

(define (breakImp statement state)
  ; Implementation of break statement
  (error "Break statement not implemented yet")
)

(define (continueImp statement state)
    ; Implementation of continue statement
    (error "Continue statement not implemented yet")
)

(define (tryImp statement state next)
  (call/cc
   (lambda (exit)
     (with-handlers
         ([(lambda (e) #t)
           (lambda (e)
             (if (null? (caddr statement))
                 (exit state)
                 (M_state (caddr statement) (addBinding state 'e e) (lambda (v) v) (lambda (v) v))))])
       (M_state (cadr statement) state (lambda (v) v) (lambda (v) v))
       (if (not (null? (cadddr statement)))
              (M_state (cadddr statement) state (lambda (v) v) (lambda (v) v))
              state)))))

(define (throwImp statement state)
    (if (null? (lookup state 'e))
        (error "Throw statement outside of try block")
        (addBinding state 'e (lookup state 'e))
    )
)

(define (assign statement state)
  (if (null? (lookup state (cadr statement)))                
      (error "Variable is not declared")
      (assign_var! (cadr statement) (M_value (caddr statement) state (lambda(v) v)) state)
  )
)

(define (declare statement state)
  (if (declared? (cadr statement) state)
      (error "Variable is already declared")
      ;whether the variable is given a initial value
      (if (null? (cddr statement))
          (add_var (cadr statement) 'value_undefined state)
          (add_var (cadr statement) (M_value (caddr statement) state (lambda(v) v)) state))))

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

(define (beginImp statements state next break)
    (if (null? statements)
            (next (pop_inner_state state)) ; pop the current scope off the stack
            (beginImp (cdr statements) (M_state (car statements) state next break) next break)))

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
    [(isBeginStatement? statement) (beginImp (cdr statement) (create_inner_state state) next break)] ; create a new scope
    [else (error "Invalid statement")]
  ))

(define (M_value statement state k)
  (cond
    [(list? statement) (map (lambda (s) (M_value s state k)) statement)]
    [(number? statement) (k statement)]
    [(var? statement) (k (find_var statement state))]
    [(eq? statement 'false) (k #f)]
    [(eq? statement 'true) (k #t)]
    [(and (eq? (car statement) '-) (null? (cddr statement))) (M_value (cadr statement) state (lambda (v) (k (* v -1))))]
    [(eq? (car statement) '-) (k (- (M_value (cadr statement) state (lambda (v) v)) (M_value (caddr statement) state (lambda (v) v))))]
    [(eq? (car statement) '+) (k (+ (M_value (cadr statement) state (lambda (v) v)) (M_value (caddr statement) state (lambda (v) v))))]
    [(eq? (car statement) '*) (k (* (M_value (cadr statement) state (lambda (v) v)) (M_value (caddr statement) state (lambda (v) v))))]
    [(eq? (car statement) '/) (k (/ (M_value (cadr statement) state (lambda (v) v)) (M_value (caddr statement) state (lambda (v) v))))]
    [(eq? (car statement) '%) (k (modulo (M_value (cadr statement) state (lambda (v) v)) (M_value (caddr statement) state (lambda (v) v))))]
    [(eq? (car statement) '>) (k (> (M_value (cadr statement) state (lambda (v) v)) (M_value (caddr statement) state (lambda (v) v))))]
    [(eq? (car statement) '>=) (k (>= (M_value (cadr statement) state (lambda (v) v)) (M_value (caddr statement) state (lambda (v) v))))]
    [(eq? (car statement) '<) (k (< (M_value (cadr statement) state (lambda (v) v)) (M_value (caddr statement) state (lambda (v) v))))]
    [(eq? (car statement) '<=) (k (<= (M_value (cadr statement) state (lambda (v) v)) (M_value (caddr statement) state (lambda (v) v))))]
    [(eq? (car statement) '!=) (k (not (eq? (M_value (cadr statement) state (lambda (v) v)) (M_value (caddr statement) state (lambda (v) v)))))]
    [(eq? (car statement) '!) (k (not (M_value (cadr statement) state (lambda (v) v ))))]
    [(eq? (car statement) '&&) (k (and (M_value (cadr statement) state (lambda (v) v)) (M_value (caddr statement) state (lambda (v) v))))]
    [(eq? (car statement) '||) (k (or (M_value (cadr statement) state (lambda (v) v)) (M_value (caddr statement) state (lambda (v) v))))]
    [(eq? (car statement) '==) (k (eq? (M_value (cadr statement) state (lambda (v) v)) (M_value (caddr statement) state (lambda (v) v))))]
    [(and (symbol? statement) (null? (lookup state statement))) (error "Variable is not declared")]
    [(symbol? statement) (k (lookup state statement))]
    [(eq? (lookup state statement) 'value_undefined) (error "Variable is not assigned a value")]
    [else (error "Invalid value")]
))

(define (execute filename)
  (statementHandler (parser filename) '(()()) (lambda (s) s)))

(parser "test.java") ; returns the parsed list of statements, for debugging purposes
(statementHandler (parser "test.java") '(()()) (lambda (s) s)) ; returns the final state, for debugging purposes