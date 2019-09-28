#lang racket

;; You can require more modules of your choice.
(require racket/string
         racket/list
         ;(prefix-in dict-c: "dict-closure.rkt")
         (prefix-in utils: "utils.rkt"))

(provide secret-word-enumeration)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                           ;;
;; Secret Word Enumeration                                                                   ;;
;; =======================                                                                   ;;
;;                                                                                           ;;
;; This step exploits the fact that all keys begin with a secret word that is a              ;;
;; proper SIX-LETTER word from the English dictionary.                                       ;;
;;                                                                                           ;;
;; Given a partial key, we can query the dictionary for a list of 6 letter words             ;;
;; that can potentially begin the key.                                                       ;;
;; We can then filter out potential candidates whose keys do not agree with our partial key. ;;
;;                                                                                           ;;
;; It is possible that given a (wrong) partial key, we discover that none of these           ;;
;; potential candidates can agree with our partial key. This really implies a                ;;
;; mistake in the choice of substitution, since dictionary-closure is completely             ;;
;; deterministic (modulo any bugs in your implementation :)                                  ;;
;;                                                                                           ;;
;; Hence, this function must return either of:                                               ;;
;; a. `#f` if there are no consistent candidates for this key.                               ;;
;; b. the original key if there are multiple consistent candidates.                          ;;
;; c. the complete key if there's only one consistent candidate!                             ;;
;;                                                                                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;main function
(define (secret-word-enumeration key-after-dictionary-closure) ;; Returns a key or false (#f)
  (let* ([word (gen-str-l key-after-dictionary-closure 6)];'("d" "I" "c" "n" "f" "c")
         [subs (loop word (length word) utils:dictionary 0 '() key-after-dictionary-closure)])
    (cond [(equal? subs 'no-match) #f]
          [(equal? subs 'multiple-match) key-after-dictionary-closure]
          [else (if (utils:is-monoalphabetic? subs key-after-dictionary-closure)
                    (utils:encryption-key (apply string-append (gen-str-l (utils:add-substitution subs key-after-dictionary-closure) (length word))))
                    #f)])))

;;turns all letters uppercase
(define (all-caps )
  '())

;;generates '("d" "I" "c" "n" "f" "c") from '(#\d #\I #\c #\n #\f #\c) of length n
(define (gen-str-l key n)
  (cond [(= n 0) '()]
        [else (cons (list->string (list (car key))) (gen-str-l (cdr key) (- n 1)))]))

;;runs through the dictionary for a match  ;;returns----'(("Y" . "k") ("S" . "c") ("C" . "s"))
(define (loop word len dict n acc key);(loop '("d" "I" "c" "n" "f" "c" "E" "c") 8 '("DISEASES" "DISPOSES") 0 '() key-after-dict-closure)
  (cond [(>= n 2) 'multiple-match]
        [(null? dict) (cond [(= n 1) acc]
                            [(= n 0) 'no-match])]
        [(equal? len (string-length (car dict))) (let ([s (subs word (transform (car dict) 0) 0 '())])
                                                   (if (equal? s #f) (loop word len (cdr dict) n acc key)
                                                       (if (similar? key (utils:encryption-key (car dict))) (loop word len (cdr dict) (+ n 1) s key)
                                                           (loop word len (cdr dict) n acc key))))]
        [else (loop word len (cdr dict) n acc key)]))

;;is key1 and key2 similar ,i.e. does k1 fit into k2?
(define (similar? k1 k2)
  (cond [(null? k1) #t]
        [(equal? (car k1) #\_) (similar? (cdr k1) (cdr k2))]
        [(equal? (car k1) (car k2)) (similar? (cdr k1) (cdr k2))]
        [else #f]))

;;returns -- (transform "apple" 0) -- '("a" "p" "p" "l" "e")
(define (transform str n)
  (cond [(equal? "" str) '()]
        [else (cons (substring str n (+ n 1)) (transform (substring str (+ n 1)) 0))]))

;;creates subs for decrypted (c-word) using dictionary (d-word)                ;'((#\B . #\i) (#\D . #\d))
(define (subs c-word d-word n acc);(subs '("w" "_" "s" "_" "o" "m") '("W" "I" "S" "D" "O" "M") '())
  (cond [(null? c-word) acc]
        [else (let* ([l1 (char-upcase (car (string->list (car c-word))))]
                     [l2 (car (string->list (car d-word)))])
                (cond [(char=? l1 #\_) (subs (cdr c-word) (cdr d-word) (+ n 1) (append acc (list (cons (integer->char (+ n 65)) (integer->char (+ 32 (char->integer l2)))))))]
                      [(equal? l1 l2) (subs (cdr c-word) (cdr d-word) (+ n 1) acc)]
                      [else #f]))]))