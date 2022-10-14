(define (sum-iter start end)  
  (define (for sum curr final)
      (if (= curr final)
          (+ sum final)
          (for (+ sum curr) (+ curr 1) final)))
  (for 0 start end)
  )
 
(define (expt-iter x n)
  (define (helper product curr)
    (if (> curr n)
        product
        (helper (* product x) (+ curr 1))))
  (helper x 2)
  )
 
(define (sum-digits number)
  (define (helper sum currNum)
    (if (< currNum 10)
         (+ sum currNum)
         (helper (+ sum (remainder currNum 10)) (quotient currNum 10))
         )
    )
  (helper 0 number)
  )

(define (reverse-didgits number)
  (define (helper sum currNum)
    (if(< currNum 10)
       (+ sum currNum)
       (helper (+ (* (remainder currNum 10) (expt 10 (didgit-count currNum 0)))) (quotient currNum 10))
    ))
    (helper 0 number)
  )
 
(define (count-divisors number)
  (define (for i counter)
    (if (= i number)
        (+ counter 1)
        (for (+ 1 i) (+ counter (if (= 0 (remainder number i)) 1 0))
        ))
    )
  (for 1 0))
 
(define (prime? number)
  (if (= (count-divisors number) 2) #t #f))

;Reverses a given number
(define (reverse-digits number)
  ;Used for counting the digits in a number
  (define (count-digits digits num)
    (if(< num 10) (+ digits 1)
    (count-digits (+ 1 digits) (quotient num 10))
    ))
  ;Used for getting the last digit and multiplying it by ten to the needed power
  (define (helper sum currNum tens)
    (if(< currNum 10) (+ sum currNum)
    (helper (+ sum (* tens (remainder currNum 10))) (quotient currNum 10) (quotient tens 10))
    ))
  (helper 0 number (expt 10 (- (count-digits 0 number) 1)))
  )

(define (increasing number)
  ;Keeps the last digit from the previous iteration
  ;and compares it to the newly modded digit to see if it is increasing
  ;if any condition breaks that -> return #f
  (define (helper currNum last-digit)
    (if (> currNum 10) 
        (if (> (remainder currNum 10) last-digit) #f
        (helper (quotient currNum 10) (remainder currNum 10)))      
    (< currNum last-digit)
    )
  )
  ;Calls helper with number and 10 since 10 is bigger than any digit 
  (helper number 10)
  )

