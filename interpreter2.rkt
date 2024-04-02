#lang racket
(require "simpleParser.rkt")
;;;*****************************************************************
;;;Reed Bower(rlb182) Haocheng He(hxh461) Walter Graham(wxg136)
;;;CSDS345
;;;*****************************************************************

; TO RUN THIS PROGRAM, mimic the following command (interpret "filename.txt") --> assuming the txt file is within the same directory
; NEEDS to be in the same directory as lex.rkt and simpleParser.rkt to work


;===============================================
;Main functions used to start and run the program


;interprets the parsed program represented as a list
(define interpret
  (lambda (filename)
    (parse (parser filename) '() return-statement)))


;main function of this program that handles the breaking up of the parsetree list created by simpleParser.rkt
;comparable to a m-state function, which the state is managed through the calling of statements (ie. var, return, if, while, =)
(define parse
  (lambda (lis state ret)
    (call/cc
     (lambda (k)
       (parse-break lis state ret k)))))


(define parse-break
  (lambda (parsetree state ret break)
    (cond
      [(null? parsetree) (ret state)]
      [(eq? (statement-type parsetree) 'var) ((declare (var-name parsetree) (expression-statement parsetree) state state (lambda (v1) (parse-break (cdr parsetree) v1 (lambda (v2) (ret v2)) break))))]
      [(eq? (statement-type parsetree) 'return) (return (car parsetree) state (lambda (v) (break v)))]
      [(eq? (statement-type parsetree) '=) (assign(expresssion-statement-assign parsetree) state (lambda (v1) (parse-break (cdr parsetree) v1 (lambda (v2) (ret v2)) break)))]
      [(eq? (statement-type parsetree) 'if) (if-statement (condition-statement parsetree) (expression-statement parsetree) state (lambda (v1) (parse (cdr parsetree) v1 (lambda (v2) (ret v2)) break )) '())]
      [(eq? (statement-type parsetree) 'while) (while-loop (condition-statement parsetree) (expression-statement parsetree) state (lambda (v1) (parse (cdr parsetree) v1 (lambda (v2) (ret v2))break)) '())]
      [(eq? (statement-type parsetree) 'try) (try-block (body parsetree) state(lambda (v1) (parse-break (cdr parsetree) v1(lambda (v2) (ret v2)) break)) '())]
      [(eq? (statement-type parsetree) 'begin) (block (body parsetree) state (lambda (v1) (parse-break (cdr parsetree) v1 (lambda (v2) (ret v2)) break)) (lambda (v3) v3) (cdr parsetree))]
      [(eq? (statement-type parsetree) 'break) (while-break (cdr parsetree) state (lambda (v1) (parse-break (cdr parsetree) v1 (lambda (v2) (ret v2)) break)) break)]
      [(eq? (statement-type parsetree) 'continue) (continue (cdr parsetree) state (lambda (v1) (parse-break (cdr parsetree) v1 (lambda (v2) (ret v2)) break)) break)]
      [else (error 'bad-keyword)])))


;===============================================
;Functions representing the statement types that will change the current state of the function (called within the main parse method)


;declare: given a variable name, expression and two state values, will iterate through the current state, and check to see if the variable was already declared
; if it gets to the end of the list, then it adds it onto the list, calculating the value of the expression with the current state
(define declare
  (lambda (name expression state prev ret)
    (if (not (pair? state))
        (error "State must be a pair")
        (cond
          [(and (null? state)(null? expression)) (ret (cons (cons name '()) state))]
          [(and (null? state) (not (null? expression))) (ret (add-value name (M-value (car expression) prev) state))]
          [(eq? name (car(car state))) (ret (error 'already-defined))]
          [else (declare name expression (cdr state) prev (lambda (v) (cons (car state) v)))]))))


;return: given an expression and state, will return the desired value (determined through the expression)
(define return
  (lambda (expression state ret)
    (cond
      [(null? expression) (ret get-value('return))]
      [(or (eq? 'true (cadr expression)) (eq? '!false (cadr expression))) (ret 'true)]
      [(or (eq? 'false (cadr expression)) (eq? '!true (cadr expression))) (ret 'false)]
      [(and (list? (cadr expression))(eq? (M-value (cadr expression) state) #t)) (ret 'true)]
      [(and (list? (cadr expression)) (eq? (M-value (cadr expression) state) #f)) (ret 'false)]
      [(list? (cadr expression)) (ret (M-value (cadr expression) state))]
      [(number? (cadr expression)) (ret (cadr expression))]
      [(eq? (get-value(cadr expression) state) #t) (ret 'true)]
      [(eq? (get-value(cadr expression) state) #f) (ret 'false)]
      [else (ret (get-value(cadr expression) state))])))


;assign: given an expression and state, assigns a value to an already declared variable within the state
; if the variable is not found within the state, an error will be displayed
(define assign
  (lambda (expression state ret)
    (cond
      [(or (null? expression) (null? state)) (ret (error 'incorrect-assignment-syntax))]
      [(not (pair? expression)) (ret (error 'incorrect-assignment-syntax))]
      [(or(list? (cadr expression)) (number? (cadr expression))) (ret (update-value (car expression) (M-value(cadr expression) state) state))]
      [else (ret (update-value (car expression) (get-value(cadr expression) state) state))])))

;block function handles begin and goto functions
(define block
  (lambda (body M-state ret break next)
    (cond
      [(null? body) (remove-first* M-state (lambda (v1) (remove-inner v1 (lambda (v2) (ret v2)))))]
      [(eq? 'break (key body)) (parse next M-state (lambda (v) v))]
      [(eq? 'return (key body)) (return (cadar body) M-state (lambda (v) (break v)))]
      [(eq? 'continue (key body)) (ret M-state)]
      [(eq? 'throw (key body)) (ret M-state)]
      [(and (eq? 'var (key body)) (not (declared*? (cdar body) M-state (lambda (v) v)))) (declare (cdar body) (cddar body) M-state M-state (lambda (v1) (block (cdr body) v1 (lambda (v2) (ret v2)) break next)))]
      [(eq? 'if (key body)) (if-statement (condition-statement body) (expression-statement body) M-state (lambda (v1) (block (cdr body) v1 (lambda (v2) (ret v2)) break next)) next)]
      [(eq? 'while (key body)) (while-loop (condition-statement body) (expression-statement body) M-state (lambda (v1) (block (cdr body) v1 (lambda (v2) (ret v2)) break next)) (cdr body))]
      [else (parse (key (car body) '()) M-state (lambda (v1) (block (cdr body) v1 (lambda (v2) (ret v2)) (lambda (v)v) next)))])))

;if-statement: takes a condition expression, statements, and state (list of bindings)
;follows the denotational semantics for if as presented in class
(define if-statement
  (lambda (condition statements state ret next)
      (cond
        [(and (M-boolean condition state (lambda (v) v)) (not (eq? 'begin (key statements)))) (insert-inner '() state (lambda (v1) (block (cons (true-statement statements) '()) v1 (lambda (v2) (ret v2)) break-statement next)))]
        [(and (not (M-boolean condition state (lambda (v) v))) (not (eq? 'begin (cdar statements)))) (insert-inner '() state (lambda (v1) (block (false-statement statements) v1 (lambda (v2) (ret v2)) break-statement next)))]
        [(and (M-boolean condition state (lambda (v) v)) (eq? 'begin (key statements))) (insert-inner '() state (lambda (v1) (block (cons (true-statement statements) '()) v1 (lambda (v2) (ret v2)) break-statement next)))]
        [else (insert-inner '() state (lambda (v1) (block (false-statement statements) v1 (lambda (v2) (ret v2)) break-statement next)))])))

; call/cc while loop function for the purpose of break and continue
(define while-loop-call-cc
  (lambda (condition body state ret next)
    (call/cc (lambda (break)
               (while-loop condition body state ret next break)))))
;while-loop: takes a loop condition expression, loop body expression, and state (list of bindings)
;follows the denotational semantics for while as presented in class 
(define while-loop
  (lambda (condition body state ret next break)
    (cond
      [(M-boolean condition state (lambda (v) (and v (eq? 'begin (key body)))))
       (insert-inner '() state (lambda (v1) (block (cdar body) v1 (lambda (v2) (while-loop condition body v2 (lambda (v3) (ret v3)) next)) break-statement next)))]
      [(M-boolean condition state (lambda (v) (and v (not (eq? 'begin (key body))))))
       (parse body state (lambda (v1) (while-loop condition body v1 (lambda (v2) (ret v2)))))]
      [else (ret state)])))

; exectutes the try block and passes the relevant information to the catch-block
(define try-block
  (lambda (lis M-state return next)
    (cond
      [(null? (car lis)) (return M-state)]
      [else (block (car lis) M-state 
                   (lambda (e) (catch-block (cdr lis) e M-state 
                                            (lambda (v2) (return v2)) next)) (lambda (v) v) next)])))

; executes the catch block and passes the relevant information to the finally-block
(define catch-block
  (lambda (lis e M-state return next)
    (cond
      ((null? (car lis)) (finally-block lis M-state (lambda (v) (return v))))
      (else (block (cddar lis) (cons (list (caadar lis) e) M-state)
                   (lambda (v1) (finally-block (cdr lis) (cdr v1) (lambda (v2) (return v2)))) (lambda (v) v) empty)))))

; finally-block: takes a finally block and M-state from executing the try-catch sequence and returns the M-state from executing the sequence
(define finally-block
  (lambda (lis M-state return)
    (cond
      [(null? (car lis)) (return M-state)]
      [else (return (block (cdar lis) M-state (lambda (v) v) empty))])))

; break: breaks out of the current loop and removes the innermost layer from the state
(define while-break
  (lambda (parsetree state ret break)
    (break (parse-break (cdr parsetree) (remove-inner state) (lambda (v) v) break))))

; continue: moves onto the next iteration of the current loop by breaking out while saving current state
(define continue
  (lambda (parsetree state ret break)
    (break (parse-break (cdr parsetree) state (lambda (v) v) break))))



;===============================================
;Essential helper functions that interact with or update the state


;adds the given variable and value binding into the current state --> equivalent to a state insert function
(define add-value-cps
  (lambda (name value state ret)
    (cond
      [(null? state) (ret (cons (cons name (cons value '())) '()))]
      [else (cons (car state) (add-value name value (cdr state) (lambda (v) v)))])))

(define add-value
  (lambda (name value state)
    (add-value-cps name value state (lambda (v)v))))


;updating a binding to the current state --> equivalent to a state update function
(define update-value-cps
  (lambda (name value state ret)
    (cond
      [(null? state) (ret (error 'using-before-declaring))]
      [(eq? name (car (car state))) (ret (cons(cons(car(car state)) (cons value '())) (cdr state)))]  
      [else (update-value name value (cdr state) (lambda (v) (cons (car state) v)))])))

(define update-value
  (lambda (name value state)
    (update-value-cps name value state (lambda (v)v))))


;returns a value from state --> equivalent to a state lookup function
(define get-value-cps
  (lambda (name state ret)
    (cond
      [(null? state) (ret (error 'using-before-declaring))]
      [(and (eq? name (car (car state))) (null? (cdr (car state)))) (ret (error 'using-before-assigning))]
      [(eq? name (car (car state))) (ret (car (cdr (car state))))]
      [else (get-value name (cdr state) (lambda (v) v))])))

(define get-value
  (lambda (name state)
    (get-value-cps name state (lambda (v)v))))

;M-value takes expressions and evaluates the components of the equation mathematically, breaking it up into left and right operand (if needed)
(define M-value
  (lambda (expression state)
    (cond
      ((number? expression) expression)
      [(and (eq? (operator expression) '-)(null? (cddr expression))) (- 0 (M-value (leftoperand expression state)state))]
      ((eq? (operator expression) '+) (+ (M-value (leftoperand expression state) state) (M-value (rightoperand expression state)state)))
      ((eq? (operator expression) '-) (- (M-value (leftoperand expression state) state) (M-value (rightoperand expression state)state)))
      ((eq? (operator expression) '*) (* (M-value (leftoperand expression state)state) (M-value (rightoperand expression state) state)))
      ((eq? (operator expression) '/) (quotient (M-value (leftoperand expression state)state) (M-value (rightoperand expression state)state)))
      ((eq? (operator expression) '%) (remainder (M-value (leftoperand expression state)state) (M-value (rightoperand expression state)state)))
      (else (M-boolean expression state)))))


    
;Takes a boolean expression and state, returning a boolean value for the expression
; works similar to that of M-value, breaking the expression up into left and right operands 
(define M-boolean
  (lambda (expression state ret)
    (cond
      [(number? expression) (ret expression)]
      [(or (eq? 'true expression) (eq? '!false expression)) (ret #t)]
      [(or (eq? 'false expression) (eq? '!true expression)) (ret #f)]
      [(eq? #t expression) (ret #t)]
      [(eq? #f expression) (ret #f)]
      [(eq? (operator expression) '!) (M-boolean (leftoperand expression state) state (lambda (v) (ret (not v))))]
      [(eq? (operator expression) '==)
       (eq? (M-boolean (leftoperand expression state) state) (M-boolean (rightoperand expression state) state return-statement))]
      [(eq? (operator expression) '!=)
       (not (eq? (M-boolean (leftoperand expression state) state) (M-boolean (rightoperand expression state) state return-statement)))]
      [(eq? (operator expression) '>=)
       (>= (M-boolean (leftoperand expression state) state) (M-boolean (rightoperand expression state) state return-statement))]
      [(eq? (operator expression) '>)
       (> (M-boolean (leftoperand expression state) state) (M-boolean (rightoperand expression state) state return-statement))]
      [(eq? (operator expression) '<)
       (< (M-boolean (leftoperand expression state) state) (M-boolean (rightoperand expression state) state return-statement))]
      [(eq? (operator expression) '<=)
       (<= (M-boolean (leftoperand expression state) state) (M-boolean (rightoperand expression state) state return-statement))]
      [(eq? (operator expression) '&&)
       (and (M-boolean (leftoperand expression state) state) (M-boolean (rightoperand expression state) state return-statement))]
      [(eq? (operator expression) '||)
       (or (M-boolean (leftoperand expression state) state) (M-boolean (rightoperand expression state) state return-statement))]
      [(arithmetic-operator? (operator expression)) (M-boolean (M-value expression state) state return-statement)]
      [else (ret (error 'bad-operator))])))


;leftoperand: function used by M-value and M-boolean to handle complex expressions (abstraction also)
; checks numerous conditions, to handle the different structures of the expression passed into it
(define leftoperand
  (lambda (expression state)
    (cond
      [(null? expression) '()]
      [(or (eq? #t (cadr expression)) (eq? #f (cadr expression))) (error 'invalid-expression)]
      [(number? (cadr expression)) (cadr expression)]
      [(list? (cadr expression)) (M-value (cadr expression) state)]
      [(and (eq? (car expression) '!) (eq? (get-value (cadr expression) state) #f)) #t]
      [(and (eq? (car expression) '!) (eq? (get-value (cadr expression) state) #t)) #f]
      [(eq? (caddr expression) 'true) #t]
      [(eq? (caddr expression) 'false) #f]
      [else (M-value (get-value(cadr expression) state) state)])))



;rightoperand: function used by M-value and M-boolean to handle complex expressions
; checks numerous conditions, to handle the different structures of the expression passed into it
(define rightoperand
  (lambda (expression state)
    (cond
      [(null? expression) '()]
      [(or (eq? #t (caddr expression)) (eq? #f (caddr expression))) (error 'invalid-expression)]
      [(number? (caddr expression)) (caddr expression)]
      [(list? (caddr expression)) (M-value (caddr expression) state)]
      [(and (eq? (car expression) '!) (eq? (get-value (cadr expression) state) #f)) #t]
      [(and (eq? (car expression) '!) (eq? (get-value (cadr expression) state) #t)) #f]
      [(eq? (caddr expression) 'true) #t]
      [(eq? (caddr expression) 'false) #f]
      [else (M-value (get-value(caddr expression) state) state)])))


;operator: abstraction for the operator of the expression
(define operator
  (lambda (expression)
    (cond
      [(null? expression) '()]
      [(eq? expression #t) 'true]
      [(eq? expression #f) 'false]
      [else (car expression)])))


;tell if a operator is an arithmetic operator
(define arithmetic-operator?
  (lambda (x)
    (cond
      [(eq? x '+) #t]
      [(eq? x '-) #t]
      [(eq? x '*) #t]
      [(eq? x '/) #t]
      [(eq? x '%) #t]
      [else #f])))

;check if is declared
(define declared*?
  (lambda (name M-state ret)
    (cond
      [(null? M-state) (ret #f)]
      [(eq? name (car M-state)) (ret #t)]
      [(list? (car M-state)) (declared*? name (car M-state) (lambda (v1) (declared*? name (cdr M-state) (lambda (v2) (ret (or v1 v2))))))]
      [else (declared*? name (cdr M-state) (lambda (v) (ret v)))])))

;  Removes the innermost empty list
(define remove-inner
  (lambda (M-state ret)
    (cond
      ((null? (car M-state)) (ret (cdr M-state)))
      (else (remove-inner (car M-state) (lambda (v) (ret (cons v (cdr M-state)))))))))

;; Removes the elements of the innermost list
(define remove-first*
  (lambda (lis ret)
    (cond
      ((null? lis) (ret lis))
      ((not (list? (car lis))) (ret empty))
      (else (remove-first* (car lis) (lambda (v) (ret (cons v (cdr lis)))))))))

;;  Adds a var name pair to the innermost layer
(define insert-inner
  (lambda (var state ret)
    (cond
      [(null? state) (ret (list var))]
      [(null? (car state)) (ret (cons (list var) (cdr state)))]
      [(not (list? (caar state))) (ret (cons var state))]
      [else (insert-inner var (car state) (lambda (v) (ret (cons v (cdr state)))))])))

;===============================================
;abstractions


;abstraction of statement type value in the leftmost sublist of parsetree (ie. var, return, =, if, while, and others that could be implemented)
(define statement-type
  (lambda (parsetree)
    (caar parsetree)))

;abstraction of variable name given the leftmost sublist of parsetree
(define var-name
  (lambda (parsetree)
    (cadar parsetree)))

;abstraction of expression statement from the parsetree's leftmost sublist
(define expression-statement
  (lambda (parsetree)
    (cddar parsetree)))

;assign's expression statement abstraction from the parsetree's leftmost sublist
(define expresssion-statement-assign
  (lambda (parsetree)
    (cdar parsetree)))

;abstraction of condition statement from the parsetree's leftmost sublist
(define condition-statement
  (lambda (parsetree)
    (cadar parsetree)))

;abstraction of condition true or false
(define true-statement car)
(define false-statement cdr)

;abstraction for break when break is just returns
(define break-statement (lambda (v) v))

;abstraction for return when it just returns
(define return-statement (lambda (v) v))

;abstraction for the statement body in if and while
(define body cdar)

;abstraction for keywork for each functions
(define key caar)


;;;*****************************************************************
;;; Helper method that might be useful later down the line..
;;;   not currently used in our working implementation
;;;*****************************************************************

;Takes an expression and M-state and replaces all variables with values stored in M-state
(define getVar
  (lambda (expression state)
    (cond
      [(null? expression) '()]
      [(not (or (or (list? expression) (number? expression))(or (eq? expression 'true) (eq? expression 'false)))) (get-value expression state)]
      [(not (or (list? (car expression))(or (number? (car expression))(or (or (eq? (car expression) 'true) (eq? (car expression) 'false)) (or (arithmetic-operator? (car expression))))))) (cons (get-value (car expression) state) (getVar (cdr expression) state))] ; <=
      [(list? (car expression)) (cons (getVar (car expression) state) (getVar (cdr expression) state))]
      [else (cons (car expression)(getVar (cdr expression) state))])))

(interpret "test.java")