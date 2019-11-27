#lang racket
(require data/pvector)
(require data/collection)
(require lens)
(require fancy-app)
(provide (all-defined-out))

(define (pvector-ref-lens i)
  (make-lens
    (nth _ i)
    (set-nth _ i _)))

(define (pvector-ref-nested-lens . is)
  (apply lens-thrush (map pvector-ref-lens is)))
