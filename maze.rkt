#lang racket
(require "../ncurses/api.rkt")
(require "struct.rkt")
(require data/pvector)
(require data/collection)
(require lens)
(require fancy-app)

(define NORTH 1)
(define SOUTH 2)
(define EAST 4)
(define WEST 8)

(define DX (hash EAST 1 WEST -1 NORTH 0 SOUTH 0))
(define DY (hash EAST 0 WEST 0 NORTH -1 SOUTH 1))

(define (opposite-of direction)
  (cond 
    [(= direction EAST) WEST]
    [(= direction WEST) EAST]
    [(= direction NORTH) SOUTH]
    [(= direction SOUTH) NORTH]))

(define (maze height width)
  (let carve-passages-from ([cx 0]
			    [cy 0]
			    [grid (make-pvector height (make-pvector width 0))])
    (for/fold ([grid grid])
      	      ([direction (shuffle (list NORTH SOUTH EAST WEST))])
      (define nx (+ cx (hash-ref DX direction)))
      (define ny (+ cy (hash-ref DY direction)))
      (cond
	[(and (<= 0 ny (sub1 height))
	      (<= 0 nx (sub1 width))
	      (= 0 (nth (nth grid ny) nx)))
	 (carve-passages-from nx
			      ny
			      (lens-transform/list grid
						   (pvector-ref-nested-lens cy cx) (bitwise-ior _ direction)
						   (pvector-ref-nested-lens ny nx) (bitwise-ior _ (opposite-of direction))))]
	[else grid]))))

(maze (random 1 10) (random 1 10))
