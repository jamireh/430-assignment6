; Assignment 3 - LOOI3
; Jonathan Amireh

#lang plai-typed
(require plai-typed/s-exp-match)
(require (typed-in racket [xor : (boolean boolean -> boolean)]))

;; Represents a number-based language with numbers, booleans, lessthanorequal, binary operators, identifiers, and applications.
(define-type ExprC
  [numC (n : number)]
  [boolC (bool : boolean)]
  [ifC (t : ExprC) (then : ExprC) (el : ExprC)]
  [binopC (op : symbol) (f : ExprC) (s : ExprC)]
  [idC (s : symbol)]
  [appC (fun : ExprC) (args : (listof ExprC))]
  [lamC (params : (listof symbol)) (body : ExprC)])
  
;; Represents a function with a name, arbitrary list of parameters and a body
(define-type FundefC
  [fdC (name : symbol) (args : (listof symbol)) (body : ExprC)])

;; Represents an evaluated value consisting of numbers, booleans, and closures.
(define-type Value
  [numV (n : number)]
  [boolV (bool : boolean)]
  [closV (args : (listof symbol)) (body : ExprC) (env : Env)])

;; Represents a local binding of a symbol to a Value
(define-type Binding
  [bind (name : symbol) (val : Value)])

;; Alias for a list of bindings, known as an Environment
(define-type-alias Env (listof Binding))
(define empty-env empty)

#|  Input: list of Bindings, Environment
    Output: Environment
    Appends the elements of the list of Bindings to the Environment
|#
(define (extend-env [append : Env] [base : Env]) : Env
  (if (empty? append) base (extend-env (rest append) (cons (first append) base))))

(test (extend-env (list (bind 'g (numV 10))) empty) (list (bind 'g (numV 10))))
(test (extend-env empty (list (bind 'g (numV 10)))) (list (bind 'g (numV 10))))
(test (extend-env empty empty) empty)

#|
   Input: first Value, second Value
   Output: boolean
   Checks whether the Values are equal; if the Values are of different varients, the function returns false.
|#
(define (eq? [f : Value] [s : Value]) : boolean
  (type-case Value f
    [numV (v) (type-case Value s
                [numV (v) (= (numV-n f) (numV-n s))]
                [else false])]
    [boolV (b) (type-case Value s
                 [boolV (b) (not (xor (boolV-bool f) (boolV-bool s)))]
                 [else false])]
    [else false]))

(test (eq? (boolV true) (boolV false)) false)
(test (eq? (boolV true) (boolV true)) true)
(test (eq? (numV 10) (numV 10)) true)
(test (eq? (numV 10) (numV 11)) false)
(test (eq? (boolV true) (closV empty (binopC '+ (numC 10) (numC 15)) empty)) false)
(test (eq? (numV 10) (closV empty (binopC '+ (numC 10) (numC 15)) empty)) false)
(test (eq? (closV empty (binopC '+ (numC 10) (numC 15)) empty) (closV empty (binopC '+ (numC 10) (numC 15)) empty)) false)

#|
   Input: symbol representing a binary operator, two Values
   Output: Value
   Accepts two numV Values and a math-based binary operator to perform and returns the result of applying that operator
|#
(define (safeNumBinopCond [op : symbol] [f : Value] [s : Value]) : Value
  (cond 
    [(symbol=? '+ op) (numV (+ (numV-n f) (numV-n s)))]
    [(symbol=? '- op) (numV (- (numV-n f) (numV-n s)))]
    [(symbol=? '/ op) (numV (/ (numV-n f) (if (= 0 (numV-n s)) (error 'safeNumBinopInterp "Cannot divide by zero") (numV-n s))))]
    [(symbol=? '* op) (numV (* (numV-n f) (numV-n s)))]
    [(symbol=? 'eq? op) (boolV (eq? f s))]
    [(symbol=? '<= op) (boolV (<= (numV-n f) (numV-n s)))]
    [else (error 'safeNumBinopCond "Unsupported binary operator")]))

(test (safeNumBinopCond '+ (numV 10) (numV 15)) (numV 25))
(test (safeNumBinopCond '- (numV 10) (numV 15)) (numV -5))
(test (safeNumBinopCond '* (numV 10) (numV 15)) (numV 150))
(test (safeNumBinopCond '/ (numV 20) (numV 10)) (numV 2))
(test/exn (safeNumBinopCond '/ (numV 20) (numV 0)) "Cannot divide by zero")
(test (safeNumBinopCond 'eq? (numV 10) (numV 15)) (boolV false))
(test (safeNumBinopCond 'eq? (numV 10) (numV 10)) (boolV true))
(test (safeNumBinopCond '<= (numV 15) (numV 10)) (boolV false))
(test (safeNumBinopCond '<= (numV 15) (numV 15)) (boolV true))
(test (safeNumBinopCond '<= (numV 15) (numV 25)) (boolV true))
(test/exn (safeNumBinopCond '^ (numV 15) (numV 25)) "Unsupported binary operator")

#|
   Input: symbol representing a binary operator, two Values
   Output: Value
   Accepts two boolV Values and a boolean-based binary operator to perform and returns the result of applying that operator
|#
(define (safeBoolBinopCond [op : symbol] [f : Value] [s : Value]) : Value
  (cond 
    [(symbol=? 'eq? op) (boolV (eq? f s))]
    [else (error 'safeBoolBinopCond "Unsupported binary operator")]))

(test (safeBoolBinopCond 'eq? (boolV true) (boolV false)) (boolV false))
(test/exn (safeBoolBinopCond 'and (boolV true) (boolV true)) "Unsupported binary operator")

#|
   Input: symbol representing a binary operator, two Values
   Output: Value
   Checks the types of the values and leverages safeNumBinopInterp and safeBoolBinopInterp to evaluate the operation
|#
(define (binopInterp [op : symbol] [f : Value] [s : Value]) : Value
  (if (symbol=? 'eq? op) (boolV (eq? f s)) 
    (type-case Value f
      [numV (n) (type-case Value s
                  [numV (n) (safeNumBinopCond op f s)]
                  [else (error 'binopInterp "Operand type mismatch")])]
      [else (error 'binopInterp "Operand type mismatch")])))

(test (binopInterp '+ (numV 10) (numV 15)) (numV 25))
(test (binopInterp '- (numV 10) (numV 15)) (numV -5))
(test (binopInterp '* (numV 10) (numV 15)) (numV 150))
(test (binopInterp '/ (numV 20) (numV 10)) (numV 2))
(test (binopInterp 'eq? (numV 10) (numV 15)) (boolV false))
(test (binopInterp 'eq? (numV 10) (numV 10)) (boolV true))
(test (binopInterp '<= (numV 15) (numV 10)) (boolV false))
(test (binopInterp '<= (numV 15) (numV 15)) (boolV true))
(test (binopInterp '<= (numV 15) (numV 25)) (boolV true))
(test (binopInterp 'eq? (boolV true) (boolV false)) (boolV false))
(test/exn (binopInterp '+ (boolV true) (boolV false)) "Operand type mismatch")
(test/exn (binopInterp '+ (numV 10) (boolV false)) "Operand type mismatch")
(test/exn (binopInterp '+ (boolV false) (numV 10)) "Operand type mismatch")
(test/exn (binopInterp '^ (numV 15) (numV 25)) "Unsupported binary operator")


#|
   Input: List of symbols, symbol to search for
   Output: boolean
   Returns true if the the list l contains the symbol s
|#

(define (contains? [l : (listof symbol)] [s : symbol]) : boolean
  (if (empty? l) false (or (symbol=? s (first l)) (contains? (rest l) s))))

(test (contains? (list 'a 'a) 'a) true)
(test (contains? (list 'a 'b 'c) 'd) false)

#|
   Input: two lists
   Output: boolean
   Returns true if all elements in a are unique, b is expected to be empty
|#
(define (unique? [a : (listof 'a)] [b : (listof 'a)]) : boolean
  (if (empty? a) true (let ([l (cons (first a) b)])
                        (and (if (contains? b (first a)) false (not (empty? l))) (unique? (rest a) l)))))

(test (unique? (list 'a 'a) empty) false)
(test (unique? (list 'a 'b 'c) empty) true)

#|
   Input: Symbol to look for, Env to search through
   Output: Value
   Searches the Env env for the symbol for; if found, it returns the Value, copied from the book
|#
(define (lookup [for : symbol] [env : Env]) : Value
  (cond
    [(empty? env) (error 'lookup "name not found")]
    [else (cond
            [(symbol=? for (bind-name (first env)))
             (bind-val (first env))]
            [else (lookup for (rest env))])]))

(test/exn (lookup 'g empty) "name not found")
(test (lookup 'g (list (bind 'g (boolV true)))) (boolV true))
(test (lookup 'f (list (bind 'g (boolV true)) (bind 'f (boolV false)))) (boolV false))

#|
   Input: s-exprssion
   Output: boolean
   Accepts an s-expression and if it evaluates to a symbol, returns whether or not it's a valid idC
|#

(define (identifier? [s : s-expression]) : boolean
  (if (s-exp-symbol? s) (let ([sym (s-exp->symbol s)]) (if (not (contains? (list '* '/ '- '+ 'func 'if 'true 'false 'fn 'with 'eq? '<=) sym)) true false)) false))

(test (identifier? `if) false)

(define (parse-identifier [s : s-expression]) : symbol
  (if (identifier? s) (s-exp->symbol s) (error 'parse "Identifier not allowed.")))

#|
   Input: s-exprssion
   Output: boolean
   Accepts an s-expression and if it evaluates to a symbol, returns whether or not it's a valid boolC
|#

(define (parse-boolean [s : s-expression]) : boolean
  (if (s-exp-symbol? s)
      (let ([sym (s-exp->symbol s)])
        (if (symbol=? 'true sym) true
            (if (symbol=? 'false sym) true false))) false))

(test (parse-boolean `true) true)
(test (parse-boolean `false) true)
(test (parse-boolean `catdog) false)

#|
   Input: lambda capable of converting an s-expression to a symbol, list of s-expressions to the apply said lambda, and a body
   Output: lamC mapping the lambda to the list whilst checking for parameter uniqueness
   Converts given argument into a function
|#
(define (gen-func [f : (s-expression -> symbol)] [l : (listof s-expression)] [body : ExprC])
  (lamC (let ([args (map f l)])
                   (if (unique? args empty) args (error 'parse "Non-unique arguments."))) body))

#|
   Input: s-expression
   Output: boolean
   Returns true if the s-expression has a valid binary operator and two operands
|#
(define (binop? [s : s-expression]) : boolean
  (if (s-exp-match? `{SYMBOL ANY ANY} s)
      (let ([sym (s-exp->symbol (first (s-exp->list s)))])
        (if (contains? (list '+ '* '/ '- 'eq? '<=) sym)
            true
            false))
      false))
#|
   Input: s-expression
   Output: binopC
   Returns a binopC if the s-expression passes the binop? test
|#
(define (parse-binop [s : s-expression]) : ExprC
  (if (binop? s)
      (let ([sl (s-exp->list s)])
        (binopC (s-exp->symbol (first sl)) (parse (second sl)) (parse (third sl))))
      (error 'parse-binop "Expression is not a binopC")))

(test/exn (parse-binop `{^ 10 2}) "Expression is not a binopC")

#|
   Input: s-expression
   Output: ExprC
   Parses the s-expression to the proper ExprC
   Supports true, false, identifiers, if, with, func, binary operators, and general expressions
|#
(define (parse [s : s-expression]) : ExprC
  (cond
    ; Primatives
    [(s-exp-number? s) (numC (s-exp->number s))]
    [(parse-boolean s) (boolC (let ([sym (s-exp->symbol s)])
                                (if (symbol=? sym 'true) true false)))]
    [(identifier? s) (idC (s-exp->symbol s))]

    ;Binary Operators
    [(binop? s)
     (parse-binop s)]

    ;Function Declaration
    [(s-exp-match? `{func ANY ... ANY} s)
     (let ([sl (s-exp->list s)])
       (gen-func (lambda (id) (parse-identifier id))
                 (reverse (rest (reverse (rest sl))))
                 (parse (first (reverse (rest sl))))))]

    ;If
    [(s-exp-match? `{if ANY ANY ANY} s)
     (let ([sl (s-exp->list s)])
       (ifC (parse (second sl)) (parse (third sl)) (parse (fourth sl))))]

    ;With
    [(s-exp-match? `{with ANY ... ANY} s)
     (let ([sl (s-exp->list s)])
       (let ([s-exp-binds (reverse (rest (reverse (rest sl))))])
         (appC (gen-func (lambda (x)
                                 (if (s-exp-match? `{SYMBOL = ANY} x)
                                     (let ([id (first (s-exp->list x))])
                                       (parse-identifier id))
                                     (error 'parse "Invalid syntax.")))
                        s-exp-binds
                        (parse (first (reverse sl))))
               (map (lambda (x) (parse (third (s-exp->list x)))) s-exp-binds))))]
    
    ;Application
    [(s-exp-match? `{ANY ANY ...} s)
      (let ([sl (s-exp->list s)])
        (appC (parse (first sl)) (map parse (rest sl))))]
    
    [else (error 'parse "Invalid syntax.")]))

(test/exn (parse `{"test"}) "Invalid syntax.")
(test/exn (parse `{with {z = {func if {10}}} {z 10}}) "Identifier not allowed.")
(test/exn (parse `{with {if = {func if {10}}} {z 10}}) "Identifier not allowed.")
(test/exn (parse `{with {if : {func if {10}}} {z 10}}) "Invalid syntax.")
(test/exn (parse `{func x x 3}) "Non-unique arguments.")
(test/exn (parse `{with {z = {func 3}} {z = 9} {z}}) "Non-unique arguments.")

#|
   Input: ExprC
   Output: Value
   Interprets the ExprC to the proper Value
|#
(define (interp [e : ExprC] [env : Env]) : Value
  (type-case ExprC e
    [numC (n) (numV n)]
    [ifC (t then el) (let ([test (interp t env)])
                          (type-case Value test
                            [boolV (bool) (if bool (interp then env) (interp el env))]
                            [else (error 'interp "Conditional is not a boolean.")]))]
    [boolC (b) (boolV b)]
    [binopC (op f s) (binopInterp op (interp f env) (interp s env))]
    [idC (n) (lookup n env)]
    [appC (f al) (local ([define f-value (interp f env)])
              (interp (closV-body f-value)
                      (extend-env (map2 bind (closV-args f-value) (map (lambda (e) (interp e env)) al)) 
                                  (closV-env f-value))))]
    [lamC (al body) (closV al body env)]
  ))

(test/exn (interp (ifC (binopC '+ (numC 5) (numC 10)) (numC 10) (numC 15)) empty) "Conditional is not a boolean.")

#|
    Input: Value
    Output: string
    Converts the value to a string
|#
(define (serialize [v : Value]) : string
  (type-case Value v
    [numV (n) (to-string n)]
    [closV (a b c) "#<procedure>"]
    [boolV (b) (if b "true" "false")]))

(test (serialize (numV 5)) "5")
(test (serialize (closV empty (numC 5) empty)) "#<procedure>")

#|
   Input: s-expression
   Output: string
   Parses and interprets the s-expression to the proper Value then converts to a string
|#
(define (top-eval [s : s-expression]) : string
  (serialize (interp (parse s) empty-env)))

(test (top-eval '{with {a = true}
                       {b = false}
                       {with {z = {func x {/ {- 2 {* 2 {+ 2 x}}} 1}}}
                             {y = 10}
                             {if {eq? {z 10} -22} {if {<= 0 {z 10}} {eq? a b} a} y}}}) "true")
(test (top-eval `(if (<= 4 3) 29387 true)) "true")
(test (top-eval `(if (<= 4 3) 29387 false)) "false")