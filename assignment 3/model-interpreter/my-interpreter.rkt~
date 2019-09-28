#lang racket
(require racket/struct)
(provide (all-defined-out))
(require "defs.rkt")
(require "examples.rkt")

(define stacks (make-vector 100))
(define stacksindex 0)

;trial
;(define c (hash 1 'a 2 'b))
;(define a (make-hash (list (cons 1 'a) (cons 2 'b))))
;(hash-set c 3 'd)
;(hash-set! a 3 'd)

;;Global definitions. A counter that tracks the framenumber
(define framenumber 0)

;The stack and its operations. I have decided to make the stack a global.
(define stack '())
(define (push frame) (set! stack (cons frame stack)))
(define (pop) (if (null? stack) (error "Empty stack")
                        (set! stack (cdr stack))))
(define (top) (if (null? stack) (error "Empty stack")
                        (car stack)))


;createframe creates a new frame. It gets its number from
;;framenumber and as a side effect increases framenumber
(define (createframe hashtable parent) ;hastable gives the initial bindings
   (let ([fr (frame framenumber hashtable parent)])
     (begin
       (set! framenumber (+ 1 framenumber))
       fr)))

;This creates the global frame, which, in our case, contains
;empty bindings.
(push (createframe (make-hash '()) (emptyframe))) 

;This interprets a program. It uses the following function processdef.
(define (eval-program prog)
         (match prog
           [(pgm deflist) (begin
                            (loafer deflist (top))
                            (return-value-of-main (top)))]))
                                 
;;processdef describes how each definition is processed and added to
;;the frame fr.
(define (processdef defn fr)
  (match defn    
    [(def v/f exp) (begin
                     (hash-set! (frame-bindings fr) v/f (eval-exp exp)))]))

;; We have said that the result of the program is the value of
;; the symbol main. main must be a defined symbol in each program.
(define (return-value-of-main frame)
  (hash-ref! (frame-bindings frame) 'main "main not found"))

;---------------------------------------------------------------------

(define (zip l1 l2)
  (cond [(or (null? l1) (null? l2)) '()]
        [else (cons (cons (car l1)
                          (car l2))
                    (zip (cdr l1) (cdr l2)))]))

(define (loafer deflist fr)
  (cond [(null? deflist) #t]
        [else (begin
                (processdef (car deflist) fr)
                (loafer (cdr deflist) fr))]))

(define (process-app exp1 explist)
  (let* ([joint (eval-exp exp1)];returns closure
         [lam-form (closure-lambda joint)];returns (lam varlist exp1)
         [arglist (lam-varlist lam-form)];returns formal arguments
         [exp-to-be-eval (lam-exp lam-form)]
         [processed-explist (map (lambda (x) (eval-exp x))
                                 explist)])
    (begin
      (push (createframe (make-hash (zip arglist processed-explist))
                         (closure-frame joint)))
      (let ([val (eval-exp exp-to-be-eval)])
        (begin
          (pop)
          val)))))
;---------------------------------------------------------------------

;; The expression evaluator is the heart of the interpreter.
;; It will use the functions below
(define (eval-exp exp)
  (cond [(symbol? exp) (let ([fr (search exp (top))])
                         (if (emptyframe? fr) (error "Symbol not found")
                             (hash-ref (frame-bindings fr) exp)))]
        [(boolean? exp) exp]
        [(number? exp) exp]
        [(list? exp) exp]
        [(string? exp) exp]
        [else (match exp 
                [(uexp op exp1) (op (eval-exp exp1))]
                [(bexp op exp1 exp2) (op (eval-exp exp1) (eval-exp exp2))]
                [(lam varlist exp1) (closure exp (top))]
                [(app exp1 explist) (process-app exp1 explist)]
                [(sett var exp1) (let* ([var-fr (search var (top))]);returns frame of var
                                   (if (emptyframe? var-fr) (error "Symbol not found")
                                       (let ([table (frame-bindings var-fr)])
                                         (hash-set! table var (eval-exp exp1)))))]                                        
                [(iff cond exp1 exp2) (if (eval-exp cond) (eval-exp exp1) (eval-exp exp2))] 
                [(lett deflist exp2) (process-lets deflist exp2)]
                [(lets deflist exp2) (if (null? deflist) (eval-exp (lett '() exp2))
                                         (eval-exp (lett (list (car deflist)) (lets (cdr deflist) exp2))))]
                [(beginexp explist) (process-beginexp explist)] 
                [(defexp deflist exp1) (begin
                                         (loafer deflist (top))
                                         (eval-exp exp1))]
                [(debugexp) (begin
                              (vector-set! stacks stacksindex stack)
                              (set! stacksindex (+ 1 stacksindex)))])]))


;;An auxilliary function that processes a begin expression
(define (process-beginexp explist)
  (match explist
    [(cons a '()) (eval-exp a)]
    [(cons a rest) (begin
                     (eval-exp a)
                     (process-beginexp rest))]))

;;An auxilliary function that processes a let expression.
;;The let definitions are in deflist, and the let body is exp.
(define (process-lets deflist exp)
  (match deflist
    ['() (begin
           (push (createframe (make-hash '()) (top)))
           (let ([v (eval-exp exp)])
             (begin
               (pop)
               v)))]
    [(cons a rest) (begin
                     (let ([empty-fr (createframe (make-hash '()) (top))])
                       (begin
                         (loafer deflist empty-fr)
                         (push empty-fr)
                         (let ([v (eval-exp exp)])
                           (begin
                             (pop)
                             v)))))]))
    

;;Prints the current environment running through a chain of frames.
;;Note that my struct definitions are such that if fr is a frame,
;;then (displayln fr) will print the frame in the format that I have
;;shown. 
(define (print-current-environment fr)
  (define (print-help f)
    (if (emptyframe? f) "Done"
        (begin
          (displayln "@@@@@@@@@@@@@@@@@@@@@@@")
          (displayln f)
          (print-help (frame-parent f)))))    
  (begin
    (print-help fr)
    (displayln "@@@@@@@@@@@@@@@@@@@@@@@")))

;;Search for the symbol sym in an environment that starts with the frame
;;fr. We shall make search return either the  emptyframe
;;or the frame containing the symbol (note, not the value of the
;;symbol itself.

(define (search sym fr)
  (cond [(emptyframe? fr) fr]
        [(hash-has-key? (frame-bindings fr) sym) fr]
        [else (search sym (frame-parent fr))]))
;-------------------------------------------------------------------------------
(define (cleanup)
  (set!  stacks (make-vector 100))
  (set! stacksindex 0)
  (set! framenumber 0)
  (set! stack '())
  (push (createframe (make-hash '()) (emptyframe))))

               


