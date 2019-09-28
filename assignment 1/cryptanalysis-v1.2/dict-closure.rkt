#lang racket/base

;; You can require more modules of your choice.
(require racket/list
         racket/string
         (prefix-in utils: "utils.rkt")
         (prefix-in stats: "statistics.rkt"))

(provide dictionary-closure)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                  ;;
;; Dictionary Closure                                                               ;;
;; ==================                                                               ;;
;;                                                                                  ;;
;; A choice of substitution can really trigger more substitutions by looking at the ;;
;; partially decrypted text - surely there will be some words which can be uniquely ;;
;; determined using the dictionary. It is prudent to concretize this "choice" as    ;;
;; this purely deterministic (involving absolutely no guess-work). In more          ;;
;; technical terms, this is called "maintaining arc-consistency" (look it up on     ;;
;; Wikipedia).                                                                      ;;
;;                                                                                  ;;
;; This function must utilise the dictionary and the cipher-word-list. Decrypt each ;;
;; word (`utils:decrypt`) and check if the dictionary has:                          ;;
;;                                                                                  ;;
;; 1. a unique completetion!                                                        ;;
;;    - Extend your key using the information with this match. Continue exploring   ;;
;;      the words under the extended key.                                           ;;
;; 2. many completions.                                                             ;;
;;    - Do nothing, just continue exploring the words under the same key. If none   ;;
;;      of the words fall into (1), return with the key that you have built so far. ;;
;; 3. no completions!                                                               ;;
;;    - Return `#f` (false), indicating that this partial key is wrong, and we must ;;
;;      revert to the original key.                                                 ;;
;;                                                                                  ;;
;; Returns either of:                                                               ;;
;; a. a (possibly) extended-key.                                                    ;;
;; b. `#f` if this key led to case (3) for some word.                               ;;
;;                                                                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;runs through the wordlist wl
(define (split wl key)
  (cond [(null? wl) key]
        [else (if (all-caps? (utils:decrypt key (car wl))) (split (cdr wl) key)
                  (let ([res (loop (transform (utils:decrypt key (car wl)) 0) (string-length (car wl)) utils:dictionary 0 '())])
                    (cond [(equal? res 'multiple-match) (split (cdr wl) key)]
                          [(equal? res 'no-match) #f]
                          [else (if (equal? res '()) (split (cdr wl) key)
                                    res)])))]))

;;all capital letters
(define (all-caps? str)
  (define l (string->list str))
  (define (helper l1)
    (cond [(null? l1) #t]
          [(char<=? (car l1) #\Z) (helper (cdr l1))]
          [else #f]))
  (helper l))

;;runs through the dictionary for a match  ;;returns----'(("Y" . "k") ("S" . "c") ("C" . "s"))
(define (loop word len dict n acc);(loop '("d" "I" "c" "n" "f" "c" "E" "c") 8 '("DISEASES" "DISPOSES") 0 '())
  (cond [(>= n 2) 'multiple-match]
        [(null? dict) (cond [(= n 1) acc]
                            [(= n 0) 'no-match])]
        [(equal? len (string-length (car dict))) (let ([s (subs word (transform (car dict) 0) '())])
                                                   (if (equal? s #f) (loop word len (cdr dict) n acc)
                                                       (loop word len (cdr dict) (+ n 1) s)))]
        [else (loop word len (cdr dict) n acc)]))

;;returns -- (transform "apple" 0) -- '("a" "p" "p" "l" "e")
(define (transform str n)
  (cond [(equal? "" str) '()]
        [else (cons (substring str n (+ n 1)) (transform (substring str (+ n 1)) 0))]))

;;creates subs for decrypted (c-word) using dictionary (d-word)
(define (subs c-word d-word acc);(subs '("d" "I" "c" "n" "f" "c" "E" "c") '("D" "I" "S" "E" "A" "S" "E" "S") '())
  (cond [(null? c-word) acc]
        [else (let* ([l1 (car (string->list (car c-word)))]
                     [l2 (car (string->list (car d-word)))])
                (cond [(char<=? l1 #\Z) (if (equal? l1 l2) (subs (cdr c-word) (cdr d-word) acc)
                                              #f)]
                      [(char>=? l1 #\a) (let ([p (present? l1 acc)]
                                                [p-prime (present-plain? l2 acc)])
                                            (cond [(equal? p l2) (subs (cdr c-word) (cdr d-word) acc)]
                                                  [(equal? p #f) (if (equal? p-prime #f) (subs (cdr c-word) (cdr d-word) (append acc (list (cons l2 l1))))
                                                                     #f)]
                                                  [else #f]))]))]))

;;checks whether cipher ch is present in acc?
(define (present? ch acc)
  (cond [(null? acc) #f]
        [(equal? ch (cdar acc)) (caar acc)]
        [else (present? ch (cdr acc))]))

;;checks whether plain ch is present in acc?
(define (present-plain? ch acc)
  (cond [(null? acc) #f]
        [(equal? ch (caar acc)) (cdar acc)]
        [else (present-plain? ch (cdr acc))]))

;;main function         
(define (dictionary-closure key)
;  (displayln key)
  (let* ([wl (utils:cipher-word-list-f utils:ciphertext)]
         [substitutions (split wl key)]);either #f or possible substitution
    (cond [(equal? #f substitutions) #f]
          [else (if (equal? substitutions key) key
                    (if (utils:is-monoalphabetic? substitutions key) (dictionary-closure (utils:add-substitution substitutions key))
                    #f))])))