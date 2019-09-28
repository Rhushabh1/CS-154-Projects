#lang racket

;; You can require more modules of your choice.
(require racket/list
         "additional-module.rkt"
         (prefix-in utils: "utils.rkt")
         (prefix-in stats: "statistics.rkt"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                     ;;
;; Strategies                                                                          ;;
;; ==========                                                                          ;;
;; For the purpose of this assignment, just the `etai` strategy is expected, since     ;;
;; we have dictionary-closure and secret-word-enumeration to leap-frog to the right    ;;
;; key. This technique would fail for harder keys which are arbitrary permutations of  ;;
;; the alphabet. We will be forced to explore many more strategies (along with         ;;
;; dictionary-closure of course).                                                      ;;
;;                                                                                     ;;
;; Strategies to guess substitutions for the key using statistical information about   ;;
;; - the English language from utils.rkt                                               ;;
;; - the cipher text      from statistics.rkt                                          ;;
;;                                                                                     ;;
;; Follow the function signature as indicated below. Deviations will make it           ;;
;; impossible for automatic grading of your submission.                                ;;
;; Moreover, we do not expect your strategies to require any more/different            ;;
;; arguments. Note that you recieve the key as argument, so you can at the very        ;;
;; least ensure that all the substitutions are monoalphabetic wrt this key.            ;;
;;                                                                                     ;;
;; Signature:                                                                          ;;
;; ```                                                                                 ;;
;; (define (my-fundoo-strategy key)                                                    ;;
;;   ;; Make use of `utils:ciphertext`, `utils:cipher-word-list`                       ;;
;;   ...)                                                                              ;;
;; ```                                                                                 ;;
;;                                                                                     ;;
;; Substitutions                                                                       ;;
;; -------------                                                                       ;;
;; In order to extend the key incrementally, we use `utils:add-substitution` to        ;;
;; extend a given key with a substitution.                                             ;;
;;                                                                                     ;;
;; A substitution is a list of pairs, each pair mapping a plaintext char to a          ;;
;; ciphertext char. For example, to extend the key with T -> a and O -> r              ;;
;; (simultaneously), we use the substitution:                                          ;;
;; ```                                                                                 ;;
;; (list (cons #\T #\a) (cons #\O #\r))                                                ;;
;; ```                                                                                 ;;
;; For a single substitution use a singleton list (containing just one pair).          ;;
;;                                                                                     ;;
;; **CAUTION**                                                                         ;;
;; -----------                                                                         ;;
;; 1. Note that add-substitution does not do sanity checks on the substitution and use ;;
;;    of `utils:is-monoalphabetic` is recommended to ensure that you don't             ;;
;;    inadvertently create invalid keys.                                               ;;
;; 2. You must provide a list called `compositions` in this module.                    ;;
;;                                                                                     ;;
;; See docs in "utils.rkt" for more information.                                       ;;
;;                                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; You must add "public" functions of this module to this list.
(provide etai
         ;; Some more suggested strategies:
         
         ;; common-words-double
         ;; bigrams
         ;; common-initial-letters
         ;; common-final-letters
         ;; common-words-triple
         ;; trigrams
         ;; common-double-letters
         ;; common-words-quadruple
         ;; quadgrams
         
         ;; lists of strategies
         composition)

;;permutations
(define (perms l)
  (if (= (length l) 1) (list l)
      (apply append (map (lambda (x) (map (lambda (y) (cons x y))
                                          (perms (remove x l))))
                         l))))

;;combines and permutes l for number of elements n
(define (nl l n)
  (cond [(= n 0) '(())]
        [(null? l) '(())]
        [else (apply append (map (lambda (x) (lc (cons x y) : y <- (nl (remove x l) (- n 1))))
                                 l))]))

;;mix l1 and l2
(define (mixer l1 l2)
  (cond [(null? l1) l2]
        [(null? l2) l1]
        [else (lc (zip l1 x) : x <- (nl l2 (length l1)))]))

;;uses the fact that E has more neighbours and T doesnt
(define (mixer2-et l3)
  (lc (list (cons #\E x) (cons #\T y)) : x <- l3 y <- (reverse l3) @(not (equal? x y)))) 

;;zip two lists : '((#\A . #\q) (#\I . #\w))
(define (zip l1 l2)
  (cond [(or (null? l1) (null? l2)) '()]
        [else (cons (cons (car l1) (car l2)) (zip (cdr l1) (cdr l2)))]))

;;string list to char list
(define (str-char l)
  (cond [(null? l) '()]
        [else (cons (car (string->list (car l))) (str-char (cdr l)))]))

;;list's first n elements
(define (list-n l n)
  (cond [(or (<= n 0) (null? l)) '()]
        [else (cons (car l) (list-n (cdr l) (- n 1)))]))

;;appending (list E T) and (list A I)
(define (appender l1 l2)
  (lc (append x y) : x <- l1 y <- l2))

;;borrows freq of mg from neighbour
(define (borrow mg neighbour)
  (cond [(null? mg) '()]
        [else (cons (cons (car mg) (scan-f (car mg) neighbour)) (borrow (cdr mg) neighbour))]))

;;
(define (scan-f ch l)
  (cond [(null? l) 0]
        [(equal? ch (caar l)) (cdar l)]
        [else (scan-f ch (cdr l))]))

;; A strategy that uses some statistical information to generate potential
;; substitutions for E, T, A and I.
;; Refer the assignment manual for tips on developing this strategy. You can
;; interact with our etai with the executable we provide.
(define (etai key)
  (let* ([mg (stats:cipher-monograms utils:ciphertext)];used          ;'(#\o #\e #\y #\w #\x #\p #\b #\c #\d #\q #\f #\u #\h #\v #\s #\m #\k #\i #\z #\n #\g #\t #\j #\r #\a)
         [com-words-1 (str-char (stats:cipher-common-words-single utils:cipher-word-list))];used         ;'(#\w #\q)
         [bg (stats:cipher-bigrams utils:cipher-word-list)];used            ;'("ep" "po" "ob" "wx")
         [neighbour (stats:cipher-unique-neighbourhood bg 'both)];used     ;'((#\o . 38) (#\w . 38) (#\y . 34) (#\q . 33))
         [pwords-1 (str-char utils:plain-common-words-single)];used        ;'(#\A #\I)
         [l2 (mixer pwords-1 com-words-1)];used                              ;'(((#\A . #\w) (#\I . #\q)) ((#\A . #\q) (#\I . #\w)))
         [sliced-n (remove* com-words-1 (list-n mg 8))];used                  ;'(#\o #\e #\y #\x)
         [freq-added (borrow sliced-n neighbour)];used                        ;'((#\o . 38) (#\e . 25) (#\y . 34) (#\x . 25))
         [l3 (stats:de-freq (stats:char-freq-sort freq-added))];used           ;'(#\o #\y #\e #\x)           ;new order wrt neighbours' freq 
         [l1 (mixer '(#\E #\T) l3)];used  (mixer2-et for efficiency)        ;;for success in the first substitution, use (sliced-n) in place of (l3)    
         [lfinal (appender l1 l2)]);used
    lfinal))  

;------------------------------------JUST FOR FUN--------------------------------------------------------------------------------------------------------------------------------
;;;scans l2 for possible match with elements in l1- (l1 - '(#\y #\o #\u #\e #\z #\c)) (l2 - '(#\o #\d #\c #\x #\e #\b #\k #\y #\n #\p))   
;(define (common-elements l1 l2);l1 is shorter in length
;  (cond [(null? l1) '()]
;        [(present? (car l1) l2) (cons (car l1) (common-elements (cdr l1) l2))]
;        [else (common-elements (cdr l1) l2)]))
;
;(define (present? x l)
;  (cond [(null? l) #f]
;        [(equal? x (car l)) #t]
;        [else (present? x (cdr l))]))
;
;;;uses the fact that E has more neighbours and T doesnt
;(define (mixer3-sed l3)
;  (lc (list (cons #\S y) (cons #\E x) (cons #\D z)) : z <- l3 x <- l3 y <- (reverse l3) @(and (not (equal? x z)) (not (equal? z y)) (not (equal? x y))))) 
;
;(define (mixer4-set lt lse)
;  (lc (list (cons #\S y) (cons #\E x) (cons #\T z)) : z <- lt x <- lse y <- lse @(and (not (equal? x z)) (not (equal? z y)) (not (equal? x y))))) 
;
;(define (set-strat key)
;  (let* ([double (stats:cipher-common-double-letters utils:cipher-word-list)]
;         [initial (stats:cipher-common-initial-letters utils:cipher-word-list)]
;         [final (stats:cipher-common-final-letters utils:cipher-word-list)]
;         [list-t (list-n initial 3)]
;         [l2 (mixer '(#\T) list-t)]
;         [l1 (mixer4-set list-t final)];[l1 (mixer3-se (remove* list-t (common-elements double final)))];[l1 (mixer3-se (remove* list-t (common-elements (list-n double 6) (list-n final 8))))]
;         [lfinal l1]);[lfinal (appender l1 l2)])
;    lfinal))
;------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

;; A suggested composition of strategies that might work well. Has not been
;; exhaustively tested by us. Be original ;)
(define composition (list etai))
                  ;; common-words-double
                  ;; bigrams
                  ;; common-initial-letters
                  ;; common-final-letters
                  ;; common-words-triple
                  ;; trigrams
                  ;; common-double-letters))
                  ;; common-words-quadruple
                  ;; quadgrams))
