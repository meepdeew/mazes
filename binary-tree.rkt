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

