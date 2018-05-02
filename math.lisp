;;; mean of two numbers
(define mean (a b) (/ (+ a b) 2))

;;; sqrt of a number (Newton's method)
(define (trysqrt guess n)                                      
  (if (> (abs (- (* guess guess) n)) 0.00001)
    (trysqrt (mean guess (/ n guess)) n)
    guess
  )
)
(define (sqrt n) (trysqrt 1.0 n))

;;; power
(define (pow base exp) (if (> exp 0) (* base (pow base (- exp 1))) 1))

;;; factorial
(define (! n) (if (> n 0) (* n (! (- n 1))) 1))
