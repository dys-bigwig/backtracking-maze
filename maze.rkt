#lang racket

(require "pvector-lens.rkt")
(require data/pvector)
(require data/collection)
(require lens)
(require fancy-app)

(define (make-grid height width)
  (make-pvector height (make-pvector width 0)))

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
			    [grid (make-grid height width)])
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


(define (no-south-wall? maze y x)
  (not (= (bitwise-and (lens-view (pvector-ref-nested-lens y x) maze)
		       SOUTH)
	  0)))

(define (no-east-wall? maze y x)
  (not (= (bitwise-and (lens-view (pvector-ref-nested-lens y x) maze)
		  EAST)
     0)) )

(define (display-maze maze height width)
  (displayln (list->string `(#\space ,@(make-list (* width 2) #\_))))
  (for ([y (in-range height)])
    (display #\|)
    (for ([x (in-range width)])
      (display (if (no-south-wall? maze y x) 
		 #\space
		 #\_))
      (if (no-east-wall? maze y x) 
	(display (if (no-south-wall? maze y (add1 x))
		   #\space
		   #\_))
	(display #\|)))
    (display #\newline)))

(display-maze (maze 20 10) 20 10)
