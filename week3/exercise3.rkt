;Applies apply-twice twice to a given function func
(define (apply-twice func arg)
  ((func (func arg))))
 
(define (square x) (* x x))
(define (succ x) (+ x 1))
 
; Creates function that applies itself twice on its argument
(define (double func)
  (lambda (x) (func (func x))))
 
;Given 2-ary function, flip the order of input arguments
(define (flip f)
  (lambda (x y) (f y x)))
 
(define weird-minus (flip -))
 
; Given 2 functions, define their composition
(define (compose f g)
  (lambda (x) (f (g x))))
 
(define (2* x) (* 2 x))

; Accumulate template for later usage
(define (accumulate operation null-value start end term next)
  (if (> start end)
      null-value
      (operation
            (term start)
            (accumulate operation null-value (next start) end term next))))
 
;Defines the function n!! (i.e. 5!! = 5.3.1 --- 6!! = 6.4.2
(define (double-factorial number)
  (accumulate * 1 (if (= (remainder number 2) 0) 2 1) number (lambda (x) x) (lambda (x) (+ x 2))))
 
;Defines func all? that checks all numbers in a range against a certain predicate?
(define (all? predicate? start end)
  (accumulate (lambda(x y) (and x y)) #t start end (lambda (x) (predicate? x)) (lambda(x) (+ x 1))))
 
;Defines func any? that checks all numbers in a range against a certain predicate?
(define (any? predicate? start end)
  (accumulate (lambda(x y) (or x y)) #f start end (lambda (x) (predicate? x)) (lambda(x) (+ x 1))))
 
; Repeats func n times on itself
(define (repeat func n)
  (accumulate compose (lambda (f) f) 1 n (lambda (x) func) (lambda (x) (+ x 1))))
 
; counts things given a predicate
(define (count predicate? start end)
  (accumulate + 0 start end (lambda (x) (if(predicate? x) 1 0)) (lambda (x) (+ x 1))))
 
; Accumulate but with filter
(define (accumulate-filter predicate? operation null-value start end term next)
  (if (> start end)
      null-value
      (operation
            (if(predicate? start)(term start) null-value)
            (accumulate-filter predicate? operation null-value (next start) end term next))))
 
;count with accumulate-filter
(define (count-filter predicate? start end)
  (accumulate-filter predicate? + 0 start end (lambda(x) 1) succ))
 
;Sums all of the divisors of number
(define (sum-divisors number)
  (accumulate-filter (lambda(x) (= (remainder number x) 0)) + 0 1 number (lambda(x) x) succ))