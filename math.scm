;;; mean of two numbers
(define (mean a b) (/ (+ a b) 2))

;;; sqrt of a number (Newton's method)
(define (trysqrt guess n)                                      
  (if (> (abs (- (* guess guess) n)) 0.00001)
    (trysqrt (mean guess (/ n guess)) n)
    guess
  )
)
(define (sqrt n) (trysqrt 1.0 n))

;;; power
(define (^ base exp) (if (> exp 0) (* base (^ base (- exp 1))) 1))

;;; factorial
(define (! n) (if (> n 0) (* n (! (- n 1))) 1))

;;; create from series up to term (limit)
(define series-limit 50)
(define (series term-generator n x) (if (> n series-limit) (term-generator n x) (+ (term-generator n x) (series term-generator (+ n 1) x))))

;;; e (using taylor series)
(define (e-term n x) (/ (^ x n) (! n)))
(define (e-pow-x x) (series e-term 0.0 x))
(define e (series e-term 0 1.0))

;;; sin (using taylor series)
(define (sin-term n x) (/ (* (^ -1 n) (^ x (+ (* 2 n) 1))) (! (+ (* 2 n) 1.0))))
(define (sin x) (series sin-term 0 x))

;;; cos (using taylor series)
(define (cos-term n x) (/ (* (^ -1 n) (^ x (* 2 n))) (! (* 2.0 n))))
(define (cos x) (series cos-term 0 x))
;;; later redefine using pi and shifted sine

;;; other trig functions defined by sine and cosine
(define (tan x) (/ (sin x) (cos x)))
(define (csc x) (/ 1 (sin x)))
(define (sec x) (/ 1 (cos x)))
(define (cot x) (/ 1 (tan x)))
