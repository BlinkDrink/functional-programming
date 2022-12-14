(define (accumulate op nv a b term next)
  (if (> a b) nv
          (op (term a) (accumulate op nv (next a) b term next))))

(define (accumulate-filter pred? op nv a b term next)
  (cond
    ((> a b) nv)
    ((pred? a) (op (term a) (accumulate-filter pred? op nv (next a) b term next)))
    (else (accumulate-filter pred? op nv (next a) b term next))))


(define (accumulate-i op nv a b term next)
  (if (> a b) nv
          (accumulate-i op (op nv (term a)) (next a) b term next)))

(define (filter p l)
  (cond ((null? l) l)
               ((p (car l)) (cons (car l) (filter p (cdr l))))
               (else (filter p (cdr l)))))

(define (foldr op nv l)
  (if (null? l) nv
          (op (car l) (foldr op nv (cdr l)))))

(define (foldl op nv l)
  (if (null? l) nv
          (foldl op (op nv (car l)) (cdr l))))

;Task 1
(define (num-divisors n)
  (accumulate + 0 1 n (lambda(x) (if(= 0 (remainder n x)) 1 0)) (lambda(x) (+ 1 x))))

(define (isPrime? n)
  (cond
    ((= n 1) #f)
    ((= 2 (num-divisors n)) #t)
    (else #f)))

(define (grow n)
  (* n (accumulate-filter
   (lambda(x) (and (= 0 (remainder n x)) (isPrime? x)))
   * 1 1 (- n 1)
   (lambda(x) x)
   (lambda(x) (+ 1 x)))))

;Task 2
(define (lists-have-common-elements? l1 l2)z
  (cond
    ((null? l1) #f)
    ((not(equal? (member (car l1) l2) #f)) #t)
    (else (lists-have-common-elements? (cdr l1) l2))))

(define (primes-of-n n)
  (accumulate-filter (lambda(x) (and (isPrime? x) (= 0 (remainder n x)))) cons '() 2 n (lambda(x) x) (lambda(x) (+ x 1))))

(define (have-common-prime-divisors? n m)
  (lists-have-common-elements? (primes-of-n n) (primes-of-n m)))

(define (maxUnitary n)
  (accumulate-filter (lambda(k) (and (= 0 (remainder n k)) (not(have-common-prime-divisors? k (quotient n k)))))
                     (lambda(x y) (if(> x y) x y)) 0 2 (- n 1) (lambda(x) x) (lambda(x) (+ x 1))))

;Task 3
(define (my-map f l)
  (if(null? l) l
     (cons (f (car l)) (my-map f (cdr l)))))

(define (checkRule1 f a b)
  (and (< (f a) (f b))
       (< (f b) (min a b))))

(define (checkRule2 f a b)
  (and (> (f b) (f a))
       (> (f a) (max a b))))

(define (selectiveMap f l1 l2)
  (define (helper lst1 lst2 lastRule res)
    (cond
      ((and (null? lst1) (null? lst2)) (append res '()))
      ((checkRule1 f (car lst1) (car lst2)) (helper (cdr lst1) (cdr lst2) 1 (append res (list (f (car lst1))))))
      ((checkRule2 f (car lst1) (car lst2)) (helper (cdr lst1) (cdr lst2) 2 (append res (list (f (car lst2))))))
      ((= 1 lastRule) (helper (cdr lst1) (cdr lst2) 1 (append res (list (f (car lst1))))))
      ((= 2 lastRule) (helper (cdr lst1) (cdr lst2) 2 (append res (list (f (car lst2))))))
      ))
  (helper (cdr l1) (cdr l2) 1 (list (f (car l1)))))

;Task 4
(define (compatible? network device)
    (>= (foldr (lambda(x y) (if(not(equal? (member x device) #f)) (+ 1 y) y)) 0 network) 2))

(define (coverage device network)
  (/ (+ (length network) (length device)) (length network)))

(define (get-compatible-devices network devices)
  (foldr (lambda(x y) (if(compatible? network x) (cons x y) y)) '() devices))

; '(1 2 5 10) '(1 5 30 40 10) => '(1 5 10)
(define (get-common-frequencies network device)
  (foldr (lambda(x y) (if(not(equal? #f (member x device))) (cons x y) y)) '() network))

(define (preferredDevice network devices)
  (if(equal? '() (get-compatible-devices network devices)) '()
  (get-common-frequencies network
   (foldr (lambda(x y) (if(> (coverage x network) (coverage y network)) x y))
   (car devices)
   (get-compatible-devices network devices))
   )))

;bonus
(define (map-networks-to-comp-devices networks devices)
  (map (lambda(x) (cons x (get-compatible-devices x devices))) networks))

(define (get-max-networks-after-map networks devices longestNetwork)
  (foldr
   (lambda(x y)
     (cond
       ((= (length (cdr x)) (length longestNetwork)) (cons x y))
       (else y)
       ))
   '() (map-networks-to-comp-devices networks devices)))

(define (longest-network lst)
  (foldr (lambda(x y) (if(> (length (cdr x)) (length (cdr y))) x y)) (car lst) lst))

(define (coverages network devices)
  (foldr (lambda (x y) (+ (coverage x network) (coverage y network))) 0 devices))

(define (get-max-coverage-device bestNetworks longestNetwork)
  (foldr (lambda(x y) (if(> (coverages (car x) (cdr x)) (coverages (car y) (cdr y))) (cdr x) (cdr y))
           (car bestNetworks) bestNetworks)))

(define (preferredDeviceForAll devices networks)
  (define longestNetwork (longest-network
                                        (map-networks-to-comp-devices networks devices)))
  (define mapped-networks
           (get-max-networks-after-map networks devices longestNetwork))
  (get-max-coverage-device mapped-networks longestNetwork))
