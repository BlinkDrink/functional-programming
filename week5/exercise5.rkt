; Fold right
(define (my-foldr func nv lst)
  (if(null? lst) nv
     (func (car lst) (my-foldr func nv (cdr lst)))))
 
; Fold left
(define (my-foldl func nv lst)
  (if(null? lst) nv
    (my-foldl func (func (car lst) nv) (cdr lst))))
 
(define (power2 a) (* a a))
 
; Map with fold
(define (my-map-fold func lst)
 (my-foldr (lambda (curr accum) (cons (func curr) accum)) '() lst))
 
; Filter with fold
(define (my-filter predicate? lst)
  (my-foldr (lambda (curr accum) (if(predicate? curr) (cons curr accum) accum)) '() lst))
 
; my min
(define (my-min lst)
  (my-foldr (lambda (curr accum) (if(< curr accum) curr accum)) (car lst) lst))
 
(define (accumulate op nv a b term next)
  (if (> a b) nv
          (op (term a) (accumulate op nv (next a) b term next))))
 
(define (generate-interval start end)
  (accumulate cons '() start end (lambda (x) x) (lambda (x) (+ 1 x))))
 
(define (zip lst1 lst2)
  (if(or (null? lst1) (null? lst2)) '()
     (cons (cons (car lst1) (car lst2)) (zip (cdr lst1) (cdr lst2)))))