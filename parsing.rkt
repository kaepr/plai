#lang plait
(print-only-errors #true)

(define-type Exp
  [numE (n : Number)]
  [boolE (b : Boolean)]
  [plusE (left : Exp) (right : Exp)]
  [cndE (test : Exp) (then : Exp) (else : Exp)])

(define-type Value
  [numV (the-number : Number)]
  [boolV (the-boolean : Boolean)])

(calc : (Exp -> Value))

(define (calc e)
  (type-case Exp e
    [(numE n) (numV n)]
    [(boolE b) (boolV b)]
    [(plusE l r) (add (calc l) (calc r))]
    [(cndE c t e) (if (boolean-decision (calc c))
                     (calc t)
                     (calc e))]))

(define (boolean-decision v)
  (type-case Value v
    [(boolV b) b]
    [else (error 'if "expects conditional to evaluate to a boolean")]))

(define (add v1 v2)
  (type-case Value v1
    [(numV n1)
     (type-case Value v2
       [(numV n2) (numV (+ n1 n2))]
       [else (error '+ "expects RHS to be a number")])]
    [else (error '+ "expects LHS to be a number")]))

(s-exp-list? `1)

(s-exp->list `{+ 1 2})

(s-exp->symbol `+)

(define (parse s)
  (cond
    [(s-exp-number? s)
     (numE (s-exp->number s))]
    [(s-exp-list? s)
     (let ([l (s-exp->list s)])
       (if (symbol=? '+
                     (s-exp->symbol (first l)))
           (plusE (parse (second l))
                 (parse (third l)))
           (error 'parse "list not an addition")))]))

(test (parse `1) (numE 1))
(test (parse `2.3) (numE 2.3))
(test (parse `{+ 1 2}) (plusE (numE 1) (numE 2)))
(test (parse `{+ 1
{+ {+ 2 3}
4}})
(plusE (numE 1)
(plusE (plusE (numE 2)
(numE 3))
(numE 4))))

(define (run s)
  (calc (parse s)))

; (test (run `1) 1)
; (test/exn (parse `{1 + 2}) "addition")