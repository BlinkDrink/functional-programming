; Problem 1
(define (accumulate op nv a b term next)
  (if (> a b) nv
      (op (term a)
          (accumulate op nv (next a) b term next))))

(define (argmin f a b)
  (accumulate (lambda (x y) (if(> (f x) (f y)) y x)) 0 a b (lambda (x) x) (lambda (x) (+ 1 x))))

(define (mod7 x) (modulo x 7))
(argmin mod7 45 50)

; Problem 2
(define (count-divs n)
  (accumulate + 0 1 n (lambda (x) (if(= (modulo n x) 0) 1 0)) (lambda (x) (+ 1 x))))

(define (count-divs-pair p)
  (if(null? p) 0 (count-divs (+ (car p) (cdr p)))))

(define (get-max-pair num b)
  (accumulate (lambda (x y) (if(> (count-divs-pair x) (count-divs-pair y)) x y)) '() (+ num 1) b (lambda(x) (cons num x)) (lambda (x) (+ 1 x))))

(define (get-max-all a b)
  (accumulate (lambda (x y) (if(> (count-divs-pair x) (count-divs-pair y)) x y)) '() a b (lambda(x) (get-max-pair x b)) (lambda(x) (+ 1 x))))


; Problem 3
(define (square x) (* x x))

(define (integral a b f dx y)
(* dx (accumulate + 0 a b (lambda (x) (f x y)) (lambda (x) (+ x dx)))))
(define (integrate2 f a b c d dx dy)
  (* dy (accumulate + 0 c d (lambda (y) (integral a b f dx y)) (lambda(y) (+ y dy)))))

(define pi 3.14159265359)
(define (f x y) (+ x (sin y) 1))
(let ((res (integrate2 f 0 2 (- pi) pi 0.01 0.01)))
  (/ res pi))

; Problem 4
(define board1 (list (cons 0 2) (cons 1 3) (cons 2 4) (cons 3 0) (cons 4 1)))
(define board2 (list (cons 0 2) (cons 1 3) (cons 2 4) (cons 3 2)))
(define board3 (list (cons 0 0) (cons 1 1) (cons 2 2) (cons 3 3) (cons 4 4)))

(define (rows-helper row n lst)
  (accumulate + 0 0 (- n 1) (lambda (x) (if(list? (member (cons row x) lst)) 1 0)) (lambda (x) (+ 1 x))))

(define (cols-helper col n lst)
  (accumulate + 0 0 (- n 1) (lambda (x) (if(list? (member (cons x col) lst)) 1 0)) (lambda (x) (+ 1 x))))

(define (n-rooks board n)
  (accumulate (lambda (x y) (and x y)) #t 0 (- n 1) (lambda(x) (if(and (= (rows-helper x n board) 1) (= (cols-helper x n board) 1)) #t #f)) (lambda(x) (+ x 1))))

(n-rooks board1 5)
(n-rooks board2 5)
(n-rooks board3 5)