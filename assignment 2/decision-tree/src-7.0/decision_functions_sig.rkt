#lang racket

;candidate functions for the toy dataset
(provide y1)
(provide y2)
(provide y3)
(provide y4>62)

(define (y1 record) (cons "feature1" (...))) ; returns the value of feature 1 for a given test sample
(define (y2 record) (cons "feature2" (...)))
(define (y3 record) (cons "feature3" (...)))
(define (y4>62 record) (cons "feature4>62" (...))) ; returns 1 if the value of feature 4 > 62, else 0

;candidate functions for the titanic dataset
(provide pclass)
(provide sex)
(provide age>25)
(provide sibsp)
(provide parch)
(provide fare>50)
(provide emb)

(define (pclass record) (cons "pclass" (...))) ; returns the value of pclass for a given test sample
(define (sex record) (cons "sex" (...)))
(define (age>25 record) (cons "age>25" (...)))
(define (sibsp record) (cons "sibsp" (...)))
(define (parch record) (cons "parch" (...)))
(define (fare>50 record) (cons "fare>50" (...)))
(define (emb record) (cons "emb" (...)))

;candidate functions for the mushroom dataset
(provide cshape)
(provide csurf)
(provide bruise)
(provide odor)
(provide gatch)
(provide gspace)
(provide gsize)
(provide sshape)
(provide nring)
(provide pop)
(provide hab)

(define (cshape record) (cons "cshape" (...)))
(define (csurf record) (cons "csurf" (...)))
(define (bruise record) (cons "bruise" (...)))
(define (odor record) (cons "odor" (...)))
(define (gatch record) (cons "gatch" (...)))
(define (gspace record) (cons "gspace" (...)))
(define (gsize record) (cons "gsize" (...)))
(define (sshape record) (cons "sshape" (...)))
(define (nring record) (cons "nring" (...)))
(define (pop record) (cons "pop" (...)))
(define (hab record) (cons "hab" (...)))
