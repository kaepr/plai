#lang stacker/smol/hof

; (defvar mk-o-static
;   (let ([counter 0])
;     (lambda (amount)
;       (begin
;         (set! counter (+ 1 counter))
;         (lambda (m)
;           (if (equal? m "get")
;             (lambda () amount)
;             (if (equal? m "count")
;               counter
;               (error "no such member")))))))
; 
; (defvar o1 (mk-o-static 1000))
; (defvar o2 (mk-o-static 0))
; (o1 "count")
; (o2 "count")

(defvar o-self!
 (let ([self 0])
  (begin
   (set!
    self
    (lambda (m)
     (if (equal? m "first")
      (lambda (x) ((self "second") (+ x 1)))
      (if 
       (equal? m "second")
       (lambda (x) (+ x 1))
       (error "no such member")))))
   self)))

((o-self! "first") 5)
       
