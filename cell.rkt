;; #lang racket
;; grid.rkt has (include "cell.rkt") && cell.rkt has #lang racket -> grid.rkt ignores cell.rkt

(require 2htdp/image)
(require uuid)

(define Cell%
  (class* object%
    (printable<%>)
    (super-new)
    
    (init-field @row @column) ;; TODO: Read-only

    (field [@id (uuid-symbol)]
           [@links (make-hash)]
           [@north '()]
           [@south '()]
           [@east '()]
           [@west '()])

    (define/public (to-s)
      (string-append
        "(Cell: " (number->string @row) ", " (number->string @column) ")"))

    (define/public (link cell (bidi #true))
      (hash-set! @links cell #true)
      (when bidi
        (send cell link this #false))
      this)
    
    (define/public (unlink cell (bidi #true))
      (hash-remove! @links cell)
      (when bidi
        (send cell unlink this #false))
      this)

    (define/public (links)
      (hash-keys @links))

    (define/public (linked? cell)
      (hash-has-key? @links cell))

    (define/public (custom-print port qqdepth)
      (write-string (to-s) port))

    (define/public (custom-write port)
      (write-string (to-s) port))

    (define/public (custom-display port)
      (write-string (to-s) port))
    
    (define/public (neighbors)
      (filter-map
       (lambda (x) (if (not (empty? x)) x #false))
       (list @north @south @east @west)))))

(define (Cell.new row column)
  (new Cell% [@row row] [@column column]))








