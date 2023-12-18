;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname edges) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;Ryan Tonucci (21059852)
;;Assignment 10

;;Q1a

;;(adj->edge adj-g) converts a graph in adjacency-list to a
;;graph in edge-list.
;;Examples
(check-expect (adj->edge '((A (C D E)) (B (E J)) (C ()) (D (F J))
                                       (E (K)) (F (K H)) (H ()) (J (H)) (K ())))
              '((A C) (A D) (A E) (B E) (B J) (D F) (D J) (E K) (F K) (F H) (J H)))
(check-expect (adj->edge '((C (D F)) (D (Y Z)) (E (F Z))))
              '((C D) (C F) (D Y) (D Z) (E F) (E Z)))
;;adj->edge: (listof (list Node (listof Node))) -> (listof (list Node Node))
;;Requires: node is a Sym

(define (adj->edge adj-g)
  (foldr append empty (map (lambda (node)
                             (map (lambda (neigbour)
                                    (append (list (first node)) (list neigbour)))
                                  (second node))) adj-g)))

;;Q1b
;;(neighbours v edge-g)  consumes a node v and a graph edge-g in edge-list
;;representation, and produces the list of out-neighbours of v in edge-g.
;;Examples
(check-expect (neighbours 'E 
                          '((A C) (A D) (A E) (B E) (B J) (D F) (D J) (E K) (F K) (F H) (J H)))
              '(K))
(check-expect (neighbours 'A 
                          '((A C) (A D) (A E) (B E) (B J) (D F) (D J) (E K) (F K) (F H) (J H)))
              '(C D E))
(check-expect (neighbours 'C 
                          '((A C) (A D) (A E) (B E) (B J) (D F) (D J) (E K) (F K) (F H) (J H)))
              '())
;;neighbours: (listof (list Node Node)) -> (listof Node)
;;Requires: node is a Sym

(define (neighbours v edge-g)
  (cond [(empty? (map (lambda (node) (second node))
       (filter (lambda (edge) (cond [(symbol=? v (first edge)) true]
                                    [else false])) edge-g)))  empty]
        [else (map (lambda (node) (second node))
       (filter (lambda (edge) (cond [(symbol=? v (first edge)) true]
                                    [else false])) edge-g))]))

;;Q1c
;;(edge->adj edge-g) converts a graph in edge-list repre- sentation to a
;;graph in adjacency-list representation
;;Examples
(check-expect (edge->adj '((C D) (C F) (D Y) (D Z) (E F) (E Z)))
              (list (list 'C (list 'D 'F)) (list 'E (list 'F 'Z))
                    (list 'D (list 'Y 'Z)) (list 'Y '())
                    (list 'F '()) (list 'Z '())))
(check-expect (edge->adj '((A C) (A D) (A E) (B E) (B J) (D F) (D J) (E K) (F K) (F H) (J H)))
              (list
 (list 'A (list 'C 'D 'E)) (list 'B (list 'E 'J))
 (list 'C '()) (list 'D (list 'F 'J)) (list 'E (list 'K))
 (list 'F (list 'K 'H)) (list 'J (list 'H)) (list 'K '()) (list 'H '())))
;;edge->adj: (listof (list Node Node)) -> (listof (list Node (listof Node)))
;;Requires: node is a Sym

(define (edge->adj edge-g)
  (foldr (lambda (node lst)
           (cond [(member? node lst) lst]
                 [else (cons node lst)])) empty
         (append (map (lambda (node) (append (list (first node))
                                         (list (neighbours (first node) edge-g)))) edge-g)
                       (map (lambda (edge) (cons (second edge) (list (neighbours (second edge) edge-g)))) edge-g))))
              







  
