;;; mean of two numbers
(define (mean a b) (/ (+ a b) 2))

;;; exact number to float
(define (float n) (+ n 0.0))

;;; sqrt of a number (Newton's method)
(define (sqrt n)
  (define tolerance 1e-10)
  (define (trysqrt guess n)                                      
    (if (> (abs (- (* guess guess) n)) tolerance)
      (trysqrt (mean guess (/ n guess)) n)
      guess))
  (float (trysqrt 1 n)))

;;; power
(define (^ base exp) (if (> exp 0) (* base (^ base (- exp 1))) 1))

;;; factorial
(define (! n) (if (> n 0) (* n (! (- n 1))) 1))

;;; create from series up to term (limit)
(define (series term-generator n x)
  (define term-size 1e-20)
  (define this-term (term-generator n x))
  (if (< (abs this-term) term-size)
    this-term
    (+ this-term
      (series term-generator (+ n 1) x))))

;;; e (using taylor series)
(define (e-term n x) (/ (^ x n) (! n)))
(define (e-pow-x x) (series e-term 0.0 x))
(define e (series e-term 0 1.0))

;;; define pi using atan (built-in)
(define pi (* 4 (atan 1)))

;;; overwrite sin (using taylor series)
(define (sin x)
  (define (sin-term n x) 
    (/ (^ x (+ (* 2 n) 1))
      (! (+ (* 2 n) 1))
      (^ -1 n)))
  (series sin-term 0 x))

;;; cos (using taylor series)
(define (cos x)
  (define (cos-term n x)
    (/
      (^ x (* 2 n))
      (! (* 2 n))
      (^ -1 n)))
  (series sin-term 0 x))
;;; alternate definition using shift of sine
; (define (cos x) (series sin-term 0 (+ x (/ pi 2))))

;;; other trig functions defined by sine and cosine
(define (tan x) (/ (sin x) (cos x)))
(define (csc x) (/ 1 (sin x)))
(define (sec x) (/ 1 (cos x)))
(define (cot x) (/ 1 (tan x)))
