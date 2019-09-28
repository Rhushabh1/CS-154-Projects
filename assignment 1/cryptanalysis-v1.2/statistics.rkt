#lang racket/base

;; You can require more modules of your choice.
(require racket/list
         racket/string
         (prefix-in utils: "utils.rkt")
         "additional-module.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                 ;;
;; Ciphertext Statistics                                                           ;;
;; =====================                                                           ;;
;;                                                                                 ;;
;; The module should provide a bunch of functions that do statistical analysis of  ;;
;; ciphertext. The output is almost always just an ordered list (counts are        ;;
;; omitted).                                                                       ;;
;;                                                                                 ;;
;; Fill in the body for the skeletons and do not change the arguments. You can     ;;
;; define as many functions as you require, there's no special credit for          ;;
;; implementing/using all of them.                                                 ;;
;;                                                                                 ;;
;; CAUTION:                                                                        ;;
;; 1. Be mindful that bi, tri and quadgrams do not cross word boundaries. Hence,   ;;
;; you must process each word separately.                                          ;;
;;                                                                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Analyses
(provide cipher-monograms
         cipher-bigrams
         cipher-unique-neighbourhood
         cipher-neighbourhood
         cipher-trigrams
         cipher-quadgrams
         cipher-common-words-single
         cipher-common-words-double
         cipher-common-words-triple
         cipher-common-words-quadruple
         cipher-common-initial-letters
         cipher-common-final-letters
         cipher-common-double-letters
         ;; any other functions of your design come below:
         char-freq
         insert-char-freq
         char-freq-sort
         de-freq
         ;; my-fundoo-analysis
         )

;;returns wordlist with each string of length 'n' from wl
(define (char-n wl n)
  (cond [(null? wl) '()]
        [(= (string-length (car wl)) n) (append (char-n (cdr wl) n) (list (car wl)))]
        [else (char-n (cdr wl) n)]))

;;returns frequency wordlist of wl (not sorted)
(define (char-freq wl)
  (cond [(null? wl) '()]
        [else (let ([y (char-freq (cdr wl))]
                    [x (car wl)])
                (insert-char-freq x y))]))

;;inserts ch in freq-wordlist wl
(define (insert-char-freq ch wl)
  (cond [(null? wl) (list (cons ch 1))]
        [else (let ([str (caar wl)]
                    [cnt (cdar wl)])
                (if (equal? ch str) (cons (cons str (+ cnt 1))
                                          (cdr wl))
                    (cons (car wl) (insert-char-freq ch (cdr wl)))))]))

;;sorts the freq-wordlist
(define (char-freq-sort wl)
  (if (null? wl) wl
      (append (char-freq-sort (lc x : x <- (cdr wl) @(> (cdr x) (cdar wl))))
              (list (car wl))
              (char-freq-sort (lc x : x <- (cdr wl) @(<= (cdr x) (cdar wl)))))))

;;de-freq the freq-wordlist wl
(define (de-freq wl)
  (cond [(null? wl) '()]
        [else (cons (caar wl) (de-freq (cdr wl)))]))

;;returns n-gram wordlist from string str with length n
(define (n-grams str n)
  (define wl (string->list str))
  (define (n-grams-help l cnt acc)
    (cond [(= cnt 0) (if (null? acc) '()
                         (list (list->string (reverse acc))))]
          [(null? l) '()]
          [else (n-grams-help (cdr l) (- cnt 1) (cons (car l) acc))]))
  (cond [(null? wl) '()]
        [else (append (n-grams-help wl n '()) (n-grams (substring str 1) n))]))

;;appends ngrams of each word of wl
(define (wl-ngram wl n)
  (cond [(null? wl) '()]
        [else (append (n-grams (car wl) n) (wl-ngram (cdr wl) n))])) 

;;predecessor-list for char ch in wl (duplicates)
(define (pre-alp-l ch wl)
  (cond [(null? wl) '()]
        [else (let* ([l (string->list (car wl))]
                     [st1 (car l)]
                     [st2 (cadr l)])
                (if (equal? st1 ch) (cons st2 (pre-alp-l ch (cdr wl)))
                    (pre-alp-l ch (cdr wl))))]))

(define (predecessor-unique ch wl)
  (rd (pre-alp-l ch wl)))

;;successor-list for char ch in wl (duplicates)
(define (suc-alp-l ch wl)
  (cond [(null? wl) '()]
        [else (let* ([l (string->list (car wl))]
                     [st1 (car l)]
                     [st2 (cadr l)])
                (if (equal? st2 ch) (cons st1 (suc-alp-l ch (cdr wl)))
                    (suc-alp-l ch (cdr wl))))]))

(define (successor-unique ch wl)
  (rd (suc-alp-l ch wl)))

;;both-list for char ch in wl (duplicates)
(define (both-alp-l ch wl)
  (append (pre-alp-l ch wl) (remove* (list ch) (suc-alp-l ch wl))))

(define (both-unique ch wl)
  (append (predecessor-unique ch wl) (remove* (list ch) (successor-unique ch wl))))

;;remove duplicate entries from a list l
(define (rd l)
  (cond [(null? l) '()]
        [else (cons (car l) (remove (car l) (rd (cdr l))))]))

;;string->char
(define (str-char l)
  (cond [(null? l) '()]
        [else (cons (car (string->list (car l))) (str-char (cdr l)))]))

;; Takes ciphertext and produces a list of cipher chars sorted in decreasing
;; order of frequency.
(define (cipher-monograms ciphertext)
  (str-char (de-freq (char-freq-sort (reverse (char-freq (wl-ngram (utils:cipher-word-list-f ciphertext) 1)))))))

;; Takes the cipher-word-list and produces a list of 2-letter bigram (strings)
;; sorted in decreasing order of frequency. Each element must be a string!
(define (cipher-bigrams cipher-word-list)
  (de-freq (char-freq-sort (reverse (char-freq (wl-ngram cipher-word-list 2))))))

;; Takes the bigram frequency order (output of `cipher-bigrams`) and computes
;; the neighbourhood of each letter with every other letter. Only unique
;; neighbours are to be counted.
;; Consider the character #\o.
;;
;; Takes an argument `mode`:
;; 1. (cipher-unique-neighbourhood cipher-bigrams 'predecessor)
;;    - Count only those unique occurrences where the (given) char preceeds
;;      other char.
;;    - Patterns of the form: "o?"
;; 2. (cipher-unique-neighbourhood cipher-bigrams 'successor)
;;    - Count only those unique occurrences where the (given) char succeeds
;;      other char.
;;    - Patterns of the form: "?o"
;; 3. (cipher-unique-neighbourhood cipher-bigrams 'both)
;;    - Count all unique occurrences where the (given) char neighbours the other
;;      char.
;;    - Patterns of the form "?o" and "o?". Note that this is not sum of (1) and
;;    (2) always.
;;
;; The output is a list of pairs of cipher char and the count of it's
;; neighbours. The list must be in decreasing order of the neighbourhood count.
(define (cipher-unique-neighbourhood cipher-bigrams-list mode)
  (cond [(equal? mode 'predecessor)  (char-freq-sort (lc (cons x (length (predecessor-unique x cipher-bigrams-list))) : x <- (string->list "abcdefghijklmnopqrstuvwxyz") ))]
        [(equal? mode 'successor)  (char-freq-sort (lc (cons x (length (successor-unique x cipher-bigrams-list))) : x <- (string->list "abcdefghijklmnopqrstuvwxyz") ))]
        [(equal? mode 'both)  (char-freq-sort (lc (cons x (length (both-unique x cipher-bigrams-list))) : x <- (string->list "abcdefghijklmnopqrstuvwxyz") ))]))
  ;; You must match against or test (using cond) for the `mode` argument. Possibilities are:
  ;; 'predecessor, 'successor, 'both
  ;; Figure out experimentally which of these is a good indicator for E vs T.

;; Takes the bigram frequency order (output of `cipher-bigrams`) and computes
;; the neighbourhood of each letter with every other letter, but counts each
;; occurrence distinctly. This comment contains 6 bigrams with "w", all with "i" or "h".
;; So "w" has:
;; when mode is 'both,        6 neighbours
;; when mode is 'predecessor, 6 neighbours
;; when mode is 'successor,   0 neighbours
(define (cipher-neighbourhood cipher-bigrams-list mode)
  (cond [(equal? mode 'predecessor)  (char-freq-sort (lc (cons x (length (pre-alp-l x cipher-bigrams-list))) : x <- (string->list "abcdefghijklmnopqrstuvwxyz") ))]
        [(equal? mode 'successor)  (char-freq-sort (lc (cons x (length (suc-alp-l x cipher-bigrams-list))) : x <- (string->list "abcdefghijklmnopqrstuvwxyz") ))]
        [(equal? mode 'both)  (char-freq-sort (lc (cons x (length (both-alp-l x cipher-bigrams-list))) : x <- (string->list "abcdefghijklmnopqrstuvwxyz") ))]))
  ;; You must match against or test (using cond) for the `mode` argument. Possibilities are:
  ;; 'predecessor, 'successor, 'both
  ;; Figure out experimentally which of these is a good indicator for E vs T.

;; Takes the cipher-word-list and produces a list of 3-letter bigram (strings)
;; sorted in decreasing order of frequency. Each element must be a string!
(define (cipher-trigrams cipher-word-list)
  (de-freq (char-freq-sort (reverse (char-freq (wl-ngram cipher-word-list 3))))))

;; Takes the cipher-word-list and produces a list of 4-letter bigram (strings)
;; sorted in decreasing order of frequency. Each element must be a string!
(define (cipher-quadgrams cipher-word-list)
  (de-freq (char-freq-sort (reverse (char-freq (wl-ngram cipher-word-list 4))))))

;; Takes the cipher word list and produces a list of single letter words, sorted
;; in decreasing order of frequency. Each element must be a string!
(define (cipher-common-words-single cipher-word-list)
  (de-freq (char-freq-sort (char-freq (char-n cipher-word-list 1)))))

;; Takes the cipher word list and produces a list of double letter words, sorted
;; in decreasing order of frequency.
(define (cipher-common-words-double cipher-word-list)
  (de-freq (char-freq-sort (char-freq (char-n cipher-word-list 2)))))

;; Takes the cipher word list and produces a list of triple letter words, sorted
;; in decreasing order of frequency.
(define (cipher-common-words-triple cipher-word-list)
  (de-freq (char-freq-sort (char-freq (char-n cipher-word-list 3)))))

;; Takes the cipher word list and produces a list of four letter words, sorted
;; in decreasing order of frequency.
(define (cipher-common-words-quadruple cipher-word-list)
  (de-freq (char-freq-sort (char-freq (char-n cipher-word-list 4)))))

;;------------------------------------------------------------------------------------------------------------

(define (initial-letters wl)
  (cond [(null? wl) '()]
        [else (cons (string-ref (car wl) 0) (initial-letters (cdr wl)))]))

(define (final-letters wl)
  (cond [(null? wl) '()]
        [else (cons (string-ref (car wl) (- (string-length (car wl)) 1)) (final-letters (cdr wl)))]))

(define (double-letters wl)
  (cond [(null? wl) '()]
        [else (let ([d (shrink-double (wl-ngram (list (car wl)) 2))])
                (if (equal? d '()) (double-letters (cdr wl))
                    (append d (double-letters (cdr wl)))))]))

(define (shrink-double lstr)
  (cond [(null? lstr) '()]
        [(equal? (string-ref (car lstr) 0) (string-ref (car lstr) 1)) (cons (string-ref (car lstr) 0) (shrink-double (cdr lstr)))]
        [else (shrink-double (cdr lstr))]))

;; Takes the cipher word list and produces a list of chars that appear at the
;; start of words, sorted in decreasing order of frequency. Each element must be
;; a char!
(define (cipher-common-initial-letters cipher-word-list)
  (de-freq (char-freq-sort (char-freq (initial-letters cipher-word-list)))))

;; Takes the cipher word list and produces a list of chars that appear at the
;; end of words, sorted in decreasing order of frequency. Each element must be
;; a char!
(define (cipher-common-final-letters cipher-word-list)
  (de-freq (char-freq-sort (char-freq (final-letters cipher-word-list)))))

;; Takes the cipher word list and produces a list of chars that appear as
;; consecutive double letters in some word, sorted in decreasing order of
;; frequency. Each element must thus be a char!
(define (cipher-common-double-letters cipher-word-list)
  (de-freq (char-freq-sort (char-freq (double-letters cipher-word-list)))))
