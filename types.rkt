#lang plait

(define-type BinOp
             [plus] [++])

(define-type Exp
             [binE (operator : BinOp)
                   (left : Exp)
                   (right : Exp)]
             [numE (value : Number)]
             [strE (value : String)])

(define-type Type [numT] [strT])

; (define-type Value [Number] [String])

(calc : (Exp -> Number))

(define (calc e)
 (type-case Exp e
  [(binE o l r)
   (type-case BinOp o
    [(plus) (+ (calc l) (calc r))]
    [(++) 0])]
    ;[(++) (string-append (calc l) (calc r))])]
  [(numE v) v]
  [(strE v) v]))

(test (calc (binE (plus) (numE 5) (numE 6))) 11)

(tc : (Exp -> Type))

(define (tc e)
  (type-case Exp e
    [(binE o l r)
     (type-case BinOp o
       [(plus) (if (and (numT? (tc l)) (numT? (tc r)))
                   (numT)
                   (error 'tc "not both numbers"))]
       [(++)   (if (and (strT? (tc l)) (strT? (tc r)))
                   (strT)
                   (error 'tc "not both strings"))])]
    [(numE v) (numT)]
    [(strE v) (strT)]))
                   

(let ([w (lambda (x) (x x))])
  (w w))




















