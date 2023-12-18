;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname goldbach) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;Ryan Tonucci (21059852)
;;Assignment 10

;;Q2a
;;(primes n) produces a list of primes list of primes that are less tham or
;;equal to n.
;;Examples
(check-expect (primes 0) empty)
(check-expect (primes 1) empty)
(check-expect (primes 20) '(2 3 5 7 11 13 17 19))
(check-expect (primes 69) '(2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67))
(check-expect (primes 19) '(2 3 5 7 11 13 17 19))
;;primes: Nat -> (listof Nat)


(define (primes n)
  (local [(define (primes-helper i lst)
            (cond
              [(>= i (floor (sqrt n))) lst]
              [else (primes-helper (+ i 2)
                                   (filter (lambda  (y)
                                             (or (not (= (modulo (+ y i) i) 0)) (= y i))) lst))]))]
    (cond [(< n 2) empty]
          [else (primes-helper 3 (filter (lambda (x) (cond [(and (even? (+ x 2)) (not (= x 2))) false]
                                               [else true]))
                             (rest (rest (build-list (add1 n) (lambda (x) x))))))])))

;;Q2b
;;(goldbach n) produces how many different ways a number can be produced
;;by adding two prime numbers.
(check-expect (goldbach 14) 2)
(check-expect (goldbach 10000) 127)
(check-expect (goldbach 658) 19)
(check-expect (goldbach 2548) 48)
(check-expect (goldbach 5632) 74)
(check-expect (goldbach 3254) 44)
(check-expect (goldbach 5612) 64)
(check-expect (goldbach 2000) 37)
;;goldbach: Nat -> Nat
;;Requires: 2 <= b <= 10000

(define (goldbach n)
  (local [(define (goldbach-helper acc lst)
            (cond [(empty? lst) acc]
                  [(member? (- n (first lst)) (rest lst)) (goldbach-helper (add1 acc) (rest lst))]
                  [else (goldbach-helper acc (rest lst))]))]
            (goldbach-helper 0 (primes n))))




