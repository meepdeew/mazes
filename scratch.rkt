#lang racket

(require 2htdp/image)

;; Title

(define (title @rows @columns)
  (let ([rows (number->string @rows)]
        [cols (number->string @columns)])
    (string-append "(Grid: " rows " x " cols ")\n")))

;; Top Row

(define (nest-n-times n a b)
  (build-list n (lambda (_) (list a b))))

(define (flatten-list nested-list)
  (foldl append empty nested-list))

(define (pairwise-list n a b)
  (flatten-list (nest-n-times n a b)))

(define (join-n-times n a b)
  (cons b (pairwise-list n a b)))

(define (top-row num top-edge corner)
  (apply beside
         (join-n-times num top-edge corner)))

;; TODO: Rewrite to just loop over cells in row the once, rendering East+South simultaneously

;; Middle Row

(define (middle-row num interior w/-wall no-wall)
  (apply beside
         (join-n-times num interior w/-wall)))

;; Bottom Row

(define (bottom-row num bottom-edge corner)
  (apply beside
         (join-n-times num bottom-edge corner)))

;; Driver

(let ([@rows    3]
      [@columns 3]
      [corner             (rectangle 10 10 "solid" "black")]
      [interior           (rectangle 50 50 "solid" "white")]
      [horizontal-w/-wall (rectangle 50 10 "solid" "black")]
      [horizontal-no-wall (rectangle 50 10 "solid" "white")]
      [vertical-w/-wall   (rectangle 10 50 "solid" "black")]
      [vertical-no-wall   (rectangle 10 50 "solid" "white")])
  ;; Print Title  
  (display (title @rows @columns))
  (above
   ;; Print Top Section
   (top-row 2
            horizontal-w/-wall
            corner)
   ;; Print Middle Section
   (middle-row 2
               interior
               vertical-w/-wall
               vertical-no-wall)
   ;; Print Bottom Section
   (bottom-row 2
               horizontal-w/-wall
               corner)))



