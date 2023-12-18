;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname engine) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;Ryan Tonucci (21059852)
;;Assignment 10

(require "players.rkt")

;;Q3
(define hand0 '(3 3 3 3 4 5 6 7 7 7 9 9 Jack Jack Queen King 2 2 Black Red))
(define hand1 
  '(4 4 4 5 5 6 6 7 8 9 10 Jack Queen King Ace 2 2))
(define hand2
  '(5 6 8 8 8 9 10 10 10 Jack Queen Queen King King Ace Ace Ace))

;;(doudizhu players hands) consumes three players and three hands and produces the winner of that game
;;Examples
(check-expect
 (doudizhu (list goldfish goldfish goldfish) (list hand0 hand1 hand2)) 'Left)
(check-expect
 (doudizhu (list reckless goldfish goldfish) (list hand0 hand1 hand2))
 'Landlord)
(check-expect
 (doudizhu (list cautious reckless goldfish) (list hand0 hand1 hand2)) 'Landlord)
;;doudizhu: (listof (Hand Role (listof Hand) -> Hand)) (listof Hand) -> Sym

(define (doudizhu players hands)
  (local [(define (doudizhu-helper turn played-cards hands) 
            (cond [(empty? (first hands)) 'Landlord]
                  [(empty? (second hands)) 'Right]
                  [(empty? (third hands)) 'Left]
                  [else (doudizhu-helper (next-player turn)
                                         (cons
                                          (((who-is-who turn) players)
                                           ((who-is-who turn) hands)
                                           turn played-cards) 
                                           played-cards)
                                         (replace-hand turn (remove-played-cards ((who-is-who turn) hands) (((who-is-who turn) players)
                                           ((who-is-who turn) hands)
                                           turn played-cards)) hands))]))]
    (doudizhu-helper 'Landlord empty hands)))


(define (replace-hand player new-hand hands)
  (cond [(symbol=? player 'Landlord) (cons new-hand (rest hands))]
        [(symbol=? player 'Right) (list (first hands) new-hand (third hands))]
        [(symbol=? player 'Left) (list (first hands) (second hands) new-hand)]))

(define (next-player current-player) ;keeps track of who should have the next turn
  (cond [(symbol=? current-player 'Landlord) 'Right]
        [(symbol=? current-player 'Right) 'Left]
        [(symbol=? current-player 'Left) 'Landlord]))

(define (who-is-who player)
  (cond [(symbol=? player 'Landlord) (lambda (lst) (first lst))]
        [(symbol=? player 'Right) (lambda (lst) (second lst))]
        [(symbol=? player 'Left) (lambda (lst) (third lst))]))



(define (remove-played-cards current-hand played-cards)
  (cond [(empty? played-cards) current-hand]
        [else (local [(define (remove-one card hand)
                        (cond [(empty? hand) empty]
                              [(equal? card (first hand)) (rest hand)]
                              [else (cons (first hand) (remove-one card (rest hand)))]))]
                (foldl remove-one current-hand played-cards))]))


(check-expect (remove-played-cards '(2 3 4 5 6) empty) '(2 3 4 5 6))
(check-expect (remove-played-cards '(3 3 3 3 4 5 6 7 7 7 9 9 Jack Jack Queen King 2 2 Black Red)
                                   '(3 3 3)) '(3 4 5 6 7 7 7 9 9 Jack Jack Queen King 2 2 Black Red))
(check-expect (remove-played-cards '(3 3 3 3 4 5 6 7 7 7 9 9 Jack Jack Queen King 2 2 Black Red)
                                   '(Jack Queen King)) '(3 3 3 3 4 5 6 7 7 7 9 9 Jack 2 2 Black Red))
