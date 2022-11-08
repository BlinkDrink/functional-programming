(define (my-length lst)
  (if (null? lst) 0
      (+ 1 (my-length (cdr lst)))))
 
(define (nth lst n)
  (if (= n 0) (car lst)
      (nth (cdr lst) (- n 1))))
 
(define (my-member element lst)
  (if (null? lst) #f
      (if (equal? element (car lst)) lst (my-member element (cdr lst)))))
 
(define (my-reverse lst)
  (if (null? lst) lst
      (append (my-reverse (cdr lst)) (list (car lst)))))
 
(define (my-take lst number)
  (define rev (my-reverse lst))
  (define newNum (- (my-length lst) number))
  (if (= newNum 0) (my-reverse rev)
      (my-take (cdr rev) (- newNum 1))))
 
(define (my-take lst number)
  (if (= 0 number) '()
      (cons (car lst) (my-take (cdr lst) (- number 1)))))
 
(define (my-drop lst number)
  (my-reverse (my-take (my-reverse lst) (- (my-length lst) number))))
 
(define (all? lst predicate?)
  (if (null? lst) #t
      (and (predicate? (car lst)) (all? (cdr lst) predicate?))))
 
(define (any? lst predicate?)
  (if (null? lst) #f
      (or (predicate? (car lst)) (any? (cdr lst) predicate?))))
 
(define (my-filter predicate? lst)
  (define (helper lst newLst)
    (cond
      ((null? lst) newLst)
      ((predicate? (car lst)) (helper (cdr lst) (append newLst (list(car lst)))))
      (else (helper (cdr lst) newLst))))
  (helper lst '()))
 
(define (remove lst element)
  (define (helper lst newLst)
  (cond
    ((null? lst) newLst)
    ((equal? (car lst) element) (helper (cdr lst) newLst))
    (else (helper (cdr lst) (append newLst (list (car lst)))))))
  (helper lst '()))
 
(define (square x) (* x x))
 
(define (my-map func lst)
  (define (helper lst newLst)
    (if(null? lst) newLst
      (helper (cdr lst) (append newLst (list (func (car lst)))))))
  (helper lst '()))