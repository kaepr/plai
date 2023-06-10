#lang stacker/smol/hof

(defvar x 5)
(deffun (set1 x y)
  (set! x y))
(set1 x 6)
x