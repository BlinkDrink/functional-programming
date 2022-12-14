;;;; Test prep
;Problem 1 almost-done numbers
(define (accumulate operation null-value start end term next)
  (if (> start end)
      null-value
      (operation
            (term start)
            (accumulate operation null-value (next start) end term next))))

(define (sum-divisors number)
  (accumulate + 0 1 (- number 1) (lambda(x)(if (=(remainder number x)0)x 0)) (lambda(x) (+ x 1))))

(define (done? number)
  (if (=(+ number 2) (sum-divisors number))
      #t
      #f))

(define (find-closest-decreasing number)
  (if (= number 0)
      0
      (if (done? number)
          number
          (find-closest-decreasing (- number 1)))))

(define (find-closest-increasing number)
  (if (done? number)
      number
      (find-closest-increasing (+ 1 number))))

(define (bigger num)
    (if (< (- (find-closest-increasing num) num) (- num (find-closest-decreasing num)))
        (- (find-closest-increasing num) num)
        (- num (find-closest-decreasing num))))

(define (check num a b)

   (if (and (< (bigger num) (- num a)) (< (bigger num) (- b num)))
       #t
       #f))

(define (almost-done-sum a b)
  (accumulate + 0 a b (lambda (x) (if (check x a b) x 0)) (lambda (x) (+ x 1))))


; Stack machine test prep problem 2
(define (my-map func lst)
  (if(null? lst) lst
     (if(number? (car lst)) (cons (func (car lst)) (my-map func (cdr lst)))
        (cons (car lst) (my-map func (cdr lst)))
        )))

(define (apply-cons-single func lst)
    (cons (func (car lst) (cadr lst)) (cddr lst)))

(define (apply-cons-func pair-input lst)
  (define func (car pair-input))
  (define times (cdr pair-input))
  (define (helper-func f t currLst)
    (cond
    ((null? currLst) currLst)
    ((and (> t 0) (> (length currLst) 1) (number? (car currLst)) (number? (cadr currLst))) (helper-func f (- t 1) (apply-cons-single f currLst)))
    (else currLst)))
  (helper-func func times lst))


(define (run-machine lst)
  (define (helper input res)
    (cond
      ((null? input) res)
      ((procedure? (car input)) (helper (cdr input) (my-map (car input) res)))
      ((pair? (car input)) (helper (cdr input) (apply-cons-func (car input) res)))
      (else (helper (cdr input) (cons (car input) res)))))
  (helper lst '()))

(run-machine (list 1 'x 4 'a 9 16 25 sqrt 6))
(run-machine (list 1 'x 4 'a 9 16 25 sqrt 6 (cons + 2) (cons * 5)))

; List is major problem 3 test prep
(define (is-major-helper? l1 l2 mainL1 mainL2)
  (cond
     ((>(length mainL1) (length mainL2)) #f )
     ((null? l1) #t)
     ((<= (car l1) (car l2)) (is-major-helper? (cdr l1) (cdr l2) mainL1 mainL2))
      (else (is-major-helper? mainL1 (cdr mainL2) mainL1 (cdr mainL2)))))

(define (is-major? lst)
  (if (or (null? lst) (= 1 (length lst)))
      #t
      (and(is-major-helper? (car lst) (cadr lst) (car lst) (cadr lst))
          (is-major? (cdr lst)))))