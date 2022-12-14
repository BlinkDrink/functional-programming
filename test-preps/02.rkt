#lang racket

;; Task 1
(define (accumulate op nv a b term next)
  (if(> a b) nv
  (op (term a)
      (accumulate op nv (next a) b term next))))

(define (foldl op nv lst)
  (if(null? lst) nv
  (foldl op (op (car lst) nv) (cdr lst))))

(define (foldr op nv lst)
  (if(null? lst) nv
  (op (car lst) (foldr op nv (cdr lst)))))

(define(product-digits number)
  (if(= number 0)  1
  (*(remainder number 10) (product-digits (quotient number 10)))))

(define (helper number)
  (abs(- number (product-digits number))))

(define (find-diff n m)
  (abs (- (helper m) (helper n))))

(define(pog n a b)
  (find-diff n (accumulate (lambda(x y) (if(>(find-diff n x) (find-diff n y) )  x y)) a a b (lambda(x) x) (lambda(x) (+ x 1)))))

(define(largest-dif a b)
 (accumulate (lambda(x y) (if (> x y) x y)) (pog a a b) a b (lambda(x) (pog x a b)) (lambda(x)(+ x 1))))

;; Task 2
(define (metric-sum lst func)
  (if (null? lst) 0
      (+ (func(car lst)) (metric-sum (cdr lst) func))))

(define (max-metric metrics lst)
  (foldl (lambda(x y) (if(> (metric-sum lst x) (metric-sum lst y)) x y)) (car metrics) metrics))

(define (prod l) (apply * l))
(define (sum l) (apply + l)) 
(max-metric (list sum prod) '((0 1 2) (3 4 5) (1337 0)))
(max-metric (list car sum)  '((1000 -1000) (29 1) (42)))



;; Task 3
(define (count-digits num)
  (if (< num 10) 1
      (+ 1 (count-digits (quotient num 10)))))

(define (get-middle number len)
  (if (= len 0)
      (remainder number 10)
      (get-middle (quotient number 10) (- len 1 ))))

(define (middle-digit num)
   (if(even? (count-digits num)) -1
           (get-middle num (quotient(count-digits num) 2))))

;; Task 4
(define (check a b op f)
  (if (= (f(op a b)) (op (f a) (f b)))
      #t
      #f))

(define (generate-pairs n lst op f)
  (foldl (lambda(x y) (and (check n x op f) y)) #t lst ))

(define (check-cond lst op f)
  (foldl (lambda(x y) (and (generate-pairs x lst op f) y)) #t lst))

(define(all-are-members f lst)
  (foldl (lambda(x y) (and (if(member  (f x) lst) #t #f) y)) #t lst))

(define (is-em? lst op f)
  (if(and(check-cond lst op f) (all-are-members f lst)) #t #f))

(is-em? '(0 1 4 6) + (lambda (x) (remainder x 3)))

; CS - 23.11.2011
; task 1
(define (accumulate-filter pred? op nv a b term next)
  (cond
    ((> a b) nv)
    ((pred? a) (op (term a) (accumulate-filter pred? op nv (next a) b term next)))
     (else (accumulate-filter pred? op nv (next a) b term next))))

(define (sum-divisors n)
  (accumulate-filter (lambda(x) (= (remainder n x) 0)) + 0 1 (quotient n 2) (lambda (x) x) (lambda(x) (+ x 1))))

(define (perfect? n)
  (= (sum-divisors n) n))

(define (sumOfPerfects a b)
  (accumulate-filter perfect? + 0 a b (lambda (x) x) (lambda(x) (+ x 1))))

;task2
;(suffix ‘(1 3 5) ‘(2 4 1 3 5)) → #t,
(define (take n l)
  (if(or (null? l) (= n 0)) '()
     (cons (car l) (take (- n 1) (cdr l)))))

(define (drop n l)
  (if(or (null? l) (= n 0)) l
     (drop (- n 1) (cdr l))))

(define (suffix l1 l2)
  (define (helper revL1 revL2)
    (equal? revL1 (take (length revL1) revL2)))
  (helper (reverse l1) (reverse l2)))

;task 3
(define (cnt-digits n)
  (if(< n 10) 1
     (+ 1 (cnt-digits (quotient n 10)))))

(define (num-to-list n)
  (if(= n 0) '()
     (cons (remainder n 10) (num-to-list (quotient n 10)))))

(define (my-comp x y)
  (if(and (number? x) (number? y)) (> x y)
     #t))

(define (is-descending l)
  (cond
    ((or (null? l) (= (length l) 1)) #t)
    ((and (> (length l) 1) (> (car l) (cadr l))) (is-descending (cdr l)))
    (else #f)))

(define (consecutiveDigits? n)
  (is-descending (num-to-list n)))

;task 4
;(fixedPoints (lambda (x) (* x x)) 0 5) → (0 1), (fixedPoints (lambda (x) (+ x 1)) 0 100) → ()

(define (fixedPoints f a b)
  (accumulate-filter (lambda(x) (= (f x) x)) cons '() a b (lambda(x) x) (lambda(x) (+ x 1))))

;task 5
(define (foldl-filter pred? op nv l)
  (cond
    ((null? l) nv)
    ((pred? (car l)) (foldl-filter pred? op (op (car l) nv) (cdr l)))
    (else (foldl-filter pred? op nv (cdr l)))))

(define (sum-list l)
  (foldl + 0 l))

(define (lowerThanGeometric l)
  (foldl-filter (lambda(x) (< x (quotient (sum-list l) (length l)))) cons '() l))

; Inf 15.11.2016
;task1
(define (negate n) (- 0 n))

(define (find-root f)
  (accumulate-filter
   (lambda(x) (if(and (= (f x) 0)) #t #f))
   + 0 (negate (expt 10 6)) (expt 10 6) (lambda(x) x) (lambda(x) (+ x 1))))

;task2
(define (lst-divides-n lst n)
  (foldl (lambda (x y) (and (= (remainder n x) 0) y)) #t lst))

(define (lst-divides-manylists-sum lst manylists)
  (cond
    ((null? manylists) #f)
    ((equal? lst (car manylists)) (lst-divides-manylists-sum lst (cdr manylists)))
    ((lst-divides-n lst (foldl + 0 (car manylists))) #t)
    (else #f)))

(define (divsum lst)
  (define (helper currLst mainLst)
    (cond
      ((null? currLst) #f)
      ((lst-divides-manylists-sum (car currLst) mainLst) #t)
      (else (helper (cdr currLst) mainLst))))
  (helper lst lst))

; task 3
; (all-increasing? ‘((1 2 3) (4 5 6) (7 8 9))) → #t

(define (get-nth-elem n l)
  (cond
    ((null? l) '())
    ((and (= n 1)) (car l))
    (else (get-nth-elem (- n 1) (cdr l)))))

(define (nth-of-each n lists)
  (if(null? lists) '()
     (cons (get-nth-elem n (car lists)) (nth-of-each n (cdr lists)))))

(define (my-cmp x y)
  (cond
    ((and (number? x) (number? y)) (> x y))
    (else #t)))

(define (ordered? lst)
      (cond ((null? lst) #t)
            ((equal? (length lst) 1) #t)
            ((> (car (cdr lst)) (car lst)) (ordered? (cdr lst)))
      (else #f))
     )

(define (all-increasing? matrix)
  (accumulate (lambda (x y) (and x y)) #t 1 (length (car matrix)) (lambda(x) (ordered? (nth-of-each x matrix))) (lambda(x) (+ 1 x))))


;task 4
(define (look-and-say lst)
  (define (helper l res final)
    (cond
      ((null? l) final)
      ((and (not(null? res)) (= (length l) 1)) (append final (list (+ (length res) 1) (car res))))
      ((= (length l) 1) (append final (list (+ (length res) 1) (car l))))
      ((equal? (car l) (cadr l)) (helper (cdr l) (cons (car l) res) final))
      ((and(not(equal? (car l) (cadr l))) (null? res)) (helper (cdr l) '() (append final (list (+ (length res) 1) (car l)))))
      (else (helper (cdr l) '() (append final (list (+ (length res) 1) (car res)))))))
  (helper lst '() '()))

(define (group-consecutive lst)
  (foldr
      (lambda (curr accum)
          (if (or (null? accum) (not (= curr (car (car accum)))))
              (cons (list curr) accum)
              (cons (cons curr (car accum)) (cdr accum))))
      '()
      lst))

(define (helperLst lst counter prey)
  (cond
    ((null? lst) (list counter prey))
    ((not (= (car lst) prey)) (append (list counter prey) (helperLst (cdr lst) 1 (car lst))))
    (else (helperLst (cdr lst) (+ 1 counter) (car lst)))))

(define (nextLookSay lst)
  (helperLst lst 0 (car lst))) 

(nextLookSay '(1 2 2 2 2 3))

;Първо контролно (2015/16 г.)

; task1
(define (filter p l)
  (cond ((null? l) l)
               ((p (car l)) (cons (car l) (filter p (cdr l))))
               (else (filter p (cdr l)))))


(define (get-last lst)
  (if(= (length lst) 1) (car lst)
     (get-last (cdr lst))))

(define (my-drop n lst)
  (if(or (= n 0) (null? lst)) lst
     (drop (- n 1) (cdr lst))))

(define (longest-descending lst)
 (define (helper mainLst currLst res)
   (cond
     ((null? mainLst) res)
     ((> (car mainLst) (get-last currLst)) (if(> (length currLst) (length res))
                                              (helper (cdr mainLst) (list (car mainLst)) currLst)
                                              (helper (cdr mainLst) (list (car mainLst)) res)))
     (else (helper (cdr mainLst) (append currLst (list (car mainLst))) res))))
  (helper (cdr lst) (list (car lst)) '()))

(longest-descending '(5 3 8 6 4 2 6 7 1))
(longest-descending '(1 2 3 4 5 6))

;task2
(define (uniq? x lst)
  (unique? x 0 lst))

(define (unique? x n lst)
  (cond
    ((and (null? lst) (= n 1)) #t)
    ((and (null? lst) (not(= n 1))) #f)
    ((= x (car lst)) (unique? x (+ n 1) (cdr lst)))
    (else (unique? x n (cdr lst)))))

(define (getFilteredLists lsts)
  (if (null? lsts) '()
      (cons (filter (lambda (x) (if(uniq? x (car lsts)) #t #f)) (car lsts)) (getFilteredLists (cdr lsts)))))

(define (getMax lst)
  (foldr (lambda (x y) (max x y)) (car lst) lst))

(define (max-unique lst)
  (foldl (lambda (x y) (if(> x y) x y)) (caar lst)))
