#lang racket

(require racket/class)
(require 2htdp/image)
(require uuid)
(require racket/include)

(include "cell.rkt")

(define Grid%
  (class* object%
    (printable<%>)
    (super-new)
    
    (init-field @rows @columns) ;; TODO: Read-only

    (field [@id (uuid-symbol)]
           [@grid (prepare-grid)]
           [then (configure-cells)])

    (define/public (prepare-grid)
      (build-list
       @rows
       (lambda (row)
         (build-list
          @columns
          (lambda (column)
            (new Cell% [@row row] [@column column]))))))

    (define/public (configure-cells)
      (each-cell (lambda (cell)
                   (let ([row (get-field @row cell)]
                         [col (get-field @column cell)])

                     (let ([neighbor-north (get-cell (- row 1) col)]
                           [neighbor-south (get-cell (+ row 1) col)]
                           [neighbor-west (get-cell row (- col 1))]
                           [neighbor-east (get-cell row (+ col 1))])

                       (when neighbor-north
                         (set-field! @north cell neighbor-north))
                       (when neighbor-south
                         (set-field! @south cell neighbor-south))
                       (when neighbor-west
                         (set-field! @west cell neighbor-west))
                       (when neighbor-east
                         (set-field! @east cell neighbor-east)))))))

    (define/public (get-cell row column)
      (if (and (>= row 0)
               (<= row (- @rows 1)))
          (if (and (>= column 0)
                   (<= column (- (length (list-ref @grid row)) 1)))
              (list-ref (list-ref @grid row) column)
              #false)
          #false))

    (define/public (random-cell)
      (let* ([row (random @rows)]
             [column (random (length (list-ref @grid row)))])
        (list-ref (list-ref @grid row) column)))
    
    (define/public (size)
      (* @rows @columns))

    (define/public (each-row cb)
      (map (lambda (row)
             (cb row)) @grid))

    (define/public (each-cell cb)
      (each-row (lambda (row)
                  (map (lambda (cell)
                         (when cell (cb cell))) row))))

    (define (title)
      (string-append "(Grid: " (number->string @rows) " x " (number->string @columns) ")\n"))
    
    (define/public (to-i)
      (let ([corner (rectangle 10 10 "solid" "black")]
            [horizontal-black (rectangle 50 10 "solid" "black")]
            [horizontal-white (rectangle 50 10 "solid" "white")]
            [vertical-black (rectangle 10 50 "solid" "black")]
            [vertical-white (rectangle 10 50 "solid" "white")]
            [interior (rectangle 50 50 "solid" "white")])
        (display (title))
        (apply
         above
         ;; make dynamic
         (beside
          corner
          horizontal-black corner
          horizontal-black corner
          horizontal-black corner)
               (each-row
                (lambda (row)
                  (above
                   ;; interiors-and-east-links
                   (beside
                    vertical-black
                    (apply beside
                           (map (lambda (cell)
                                  (if (send cell linked?
                                            (get-field @east cell))
                                      (beside interior vertical-white)
                                      (beside interior vertical-black)))
                                row))
                    )
                   ;; bottoms-and-south-links
                   (beside
                    corner
                    (apply beside
                           (map (lambda (cell)
                                  (if (send cell linked?
                                            (get-field @south cell))
                                      (beside horizontal-white corner)
                                      (beside horizontal-black corner)))
                                row)))))))))

    (define/public (to-s)
      (apply string-append
             (title)
             "+---+---+---+\n"
             (each-row
              (lambda (row)
                (string-append
                 "|"
                 (apply string-append
                        (map (lambda (cell)
                               (if (send cell linked?
                                         (get-field @east cell))
                                   "    "
                                   "   |")) row)
                        )
                 "\n"
                 "+"
                 (apply string-append
                        (map (lambda (cell)
                               (if (send cell linked?
                                         (get-field @south cell))
                                   "   +"
                                   "---+")) row)
                        )
                 "\n")))))

    (define/public (custom-print port qqdepth)
      (write-string (to-s) port))

    (define/public (custom-write port)
      (write-string (to-s) port))

    (define/public (custom-display port)
      (write-string (to-s) port))))

(define (Grid.new rows columns)
  (new Grid% [@rows rows] [@columns columns]))

(define grid1 (Grid.new 3 3))

(define cell1 (send grid1 get-cell 1 1))
(define cell2 (send grid1 get-cell 1 2))
(define cell3 (send grid1 get-cell 2 2))

(send cell1 link cell2)
(send cell2 link cell3)
grid1
(send grid1 to-i)

#|  Grid Structure (row, column):

'(((0 0) (0 1) (0 2))
  ((1 0) (1 1) (1 2))
  ((2 0) (2 1) (2 2)))

|#
