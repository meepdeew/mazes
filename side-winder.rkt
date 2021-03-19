#lang racket

(include "grid.rkt")

(define grid1 (Grid.new 3 3))

(define (top-bottom)
  (let ([coin-flip (random 2)])
    (cond [(= coin-flip 0) #true]
          [(= coin-flip 1) #false])))

(define (not-empty? maybe-cell)
  (not (empty? maybe-cell)))

(define (safe-link cell neighbor)
  (when (not-empty? neighbor)
    (send cell link neighbor)))

(define (side-winder grid)
  (send grid each-row
        (lambda (row)
          (build-list (get-field @columns grid) (lambda (col) (top-bottom)))
          ; 1 (list t t t t) -> (list (t t t t))
          ; 2 (list t t t f) -> (list (t t t t))
          ; 3 (list t t f t) -> (list (t t t) (t))
          ; 4 (list t f t t) -> (list (t t) (t t))
          ; 5 (list f t t t) -> (list (t) (t t t))
          ; 6 (list t t f f) -> (list (t t t) (t))
          ; 7 (list t f t f) -> (list (t t) (t t))
          ; 8 (list f t t f) -> (list (t) (t t t))
          ; 9 (list t f f t) -> (list (t t) (t) (t))
          ; 0 (list f t f t) -> (list (t) (t t) (t))
          ; 1 (list f f t t) -> (list (t) (t) (t t))
          ; 2 (list t f f f) -> (list (t t) (t) (t))
          ; 3 (list f t f f) -> (list (t) (t t) (t))
          ; 4 (list f f t f) -> (list (t) (t) (t t))
          ; 5 (list f f f t) -> (list (t) (t) (t) (t))
          ; 6 (list f f f f) -> (list (t) (t) (t) (t))

          ; 1 -> (list 4)
          ; 2 -> (list 3 1)
          ; 3 -> (list 2 2)
          ; 4 -> (list 1 3)
          ; 5 -> (list 2 1 1)
          ; 6 -> (list 1 2 1)
          ; 7 -> (list 1 1 2)
          ; 8 -> (list 1 1 1 1)

          ;; break the row into n groups
          ;; flip heads each time to decide whether 
          ...)))

(define (binary-tree grid)
  (send grid each-cell
        (lambda (cell)
          (let ([n-north (get-field @north cell)]
                [n-east  (get-field @east cell)]
                [row (get-field @row cell)]
                [rows (get-field @rows grid)]
                [column (get-field @column cell)]
                [columns (get-field @columns grid)])
            (cond
              [(and (= row 0) (not (= column (- columns 1))))
               (safe-link cell n-east)]
              [(and (= column (- columns 1)) (not (= row 0)))
               (safe-link cell n-north)]
              [else (if (top-bottom)
                (safe-link cell n-north)
                (safe-link cell n-east))]))))
  grid)

grid1
(binary-tree grid1)
