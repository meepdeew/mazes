#lang racket

(require racket/class)
(require 2htdp/image)

;;; stuff for drawing non-text mazes, tried before reading

(define cell-width 50)
(define (draw-cell top-linked bottom-linked left-linked right-linked)
  (let ([exterior (rectangle (+ 0 cell-width) (+ 0 cell-width) "solid" "black")]
        [interior (build-interior top-linked bottom-linked left-linked right-linked)])
    (overlay/align
     "center"
     "center"
     interior
     exterior)))

(define (build-interior top-p bottom-p left-p right-p)
  (let ([interior (rectangle 40 40 "solid" "white")]
        [left (rectangle 5 40 "solid" (if left-p "white" "black"))]
        [right (rectangle 5 40 "solid" (if right-p "white" "black"))]
        [top (rectangle 40 5 "solid" (if top-p "white" "black"))]
        [bottom (rectangle 40 5 "solid" (if bottom-p "white" "black"))])
    (above
     top
     (beside left interior right)
     bottom)))

(define (draw-grid cells)
  (overlay/align
   "center"
   "center"
   cells
   (rectangle (+ 10 (* 3 cell-width)) (+ 10 (* 3 cell-width)) "solid" "black")))

; top bottom left right

(draw-grid (above (beside (draw-cell #false #false #false #false)
                          (draw-cell #false #false #false #false)
                          (draw-cell #false #false #false #false))
                  (beside (draw-cell #false #false #false #false)
                          (draw-cell #false #false #false #true)
                          (draw-cell #false #true #true #false))
                  (beside (draw-cell #false #false #false #false)
                          (draw-cell #false #false #false #false)
                          (draw-cell #true #false #false #false))))




; instantiate a 10x10 grid
; grid = Grid.new (10, 10)
; (define grid (new Grid% [size 10 10]))

; get the cell at row 1, column 2
; cell = grid [1, 2]
; (define cell (send grid get 1 2))

; iterate over all the cells in the grid
; grid.each_cell do |cell|
  ; do something to the cell
; end
; (send grid each_cell (lambda (cell) ...))


;; How to make hash map in Racket?
