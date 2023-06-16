#lang racket
(require [only-in plait test print-only-errors])

(define o
  (lambda (m)
    (case m
      [(add1) (lambda (x) (+ x 1))]
      [(sub1) (lambda (x) (- x 1))])))

(test ((o 'add1) 5) 6)

(define (msg o m . a)
  (apply (o m) a))

(test (msg o 'add1 5) 6)

(define (o-constr x)
  (lambda (m)
    (case m
      [(addX) (lambda (y) (+ x y))])))

(test (msg (o-constr 5) 'addX 3) 8)


(define (mk-o-state count)
  (lambda (m)
    (case m
      [(inc) (lambda () (set! count (+ count 1)))]
      [(dec) (lambda () (set! count (- count 1)))]
      [(get) (lambda () count)])))

(test (let ([o (mk-o-state 5)])
        (begin 
          (msg o 'inc)
          (msg o 'inc)
          (msg o 'dec)
          (msg o 'get)))
      6)

(define (mk-o-state/priv init)
  (let ([count init])
    (lambda (m)
        (case m
          [(inc) (lambda () (set! count (+ count 1)))]
          [(dec) (lambda () (set! count (- count 1)))]
          [(get) (lambda () count)]))))

(define mk-o-static 
  (let ([counter 0])
    (lambda (amount)
      (begin
        (set! counter (+ 1 counter))
        (lambda (m)
          (case m
            [(inc) (lambda (n) (set! amount (+ amount n)))]
            [(dec) (lambda (n) (set! amount (- amount n)))]
            [(get) (lambda () amount)]
            [(count) (lambda () counter)]))))))

(test (let ([o (mk-o-static 1000)])
        (msg o 'count))
      1)

(test (let ([o (mk-o-static 0)])
        (msg o 'count))
      2)

(define o-self!
  (let ([self 'dummy])
    (begin
      (set! 
        self
        (lambda (m)
          (case m
            [(first) (lambda (x) (msg self 'second (+ x 1)))]
            [(second) (lambda (x) (+ x 1))])))
     self)))

(test (msg o-self! 'first 5) 7)

(define (mt)
  (let ([self 'dummy])
    (begin
      (set! self
        (lambda (m)
          (case m
            [(sum) (lambda () 0)])))
      self)))

(define (node v l r)
  (let ([self 'dummy])
    (begin
      (set! self
        (lambda (m)
          (case m
            [(sum) (lambda () (+ v
                                 (msg l 'sum)
                                 (msg r 'sum)))])))
      self)))
          
(define a-tree
  (node 10
        (node 5 (mt) (mt))
        (node 15 (node 6 (mt) (mt)) (mt))))

(test (msg a-tree 'sum) (+ 10 5 15 6))

(define (node/size parent-maker v l r)
  (let ([parent-object (parent-maker v l r)]
        [self 'dummy])
    (begin
      (set! 
        self
        (lambda (m)
          (case m
            [(size) (lambda () (+ 1
                                  (msg l 'size)
                                  (msg r 'size)))]
            [else (parent-object m)])))
      self)))

(define (mt/size parent-maker)
  (let ([parent-object (parent-maker)]
        [self 'dummy])
    (begin
      (set!
        self
        (lambda (m)
          (case m
            [(size) (lambda () 0)]
            [else (parent-object m)])))
      self)))

(define a-tree/size
  (node/size node
             10
             (node/size node 5 (mt/size mt) (mt/size mt))
             (node/size node 15
                        (node/size node 6 (mt/size mt) (mt/size mt))
                        (mt/size mt))))

(test (msg a-tree/size 'sum) (+ 10 5 15 6))
(test (msg a-tree/size 'size) 4)


        
