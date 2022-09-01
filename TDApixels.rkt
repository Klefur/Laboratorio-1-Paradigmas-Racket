#lang racket

; constructores

(define (pixbit-d x y bit d) (list x y bit d))

(define (pixrgb-d x y r g b d) (list x y (list r g b) d))

(define (pixhex-d x y hex d) (list x y hex d))

; pertenencia

(define (pixbit-d? pixel)
  (if (list? pixel)
      (if (= (length pixel) 4)
          (or (= (caddr pixel) 1) (= (caddr pixel) 0))
          #f)
      #f))

(define (pixrgb-d? pixel)
  (if (list? pixel)
      (if (= (length pixel) 6)
          (andmap (lambda (c) (if (and (c >= 0) (c <= 255))
                                  #t
                                  #f)) (caddr pixel))
          #f)
      #f))

(define (pixhex-d? pixel)
  (if (list? pixel)
      (if (= (length pixel) 4)
          (if (string? (caddr pixel))
              (and (= (car pixel) "#") (= (length pixel) 7))
              #f)
          #f)
      #f))

;selectores

(define (getPosX pixel)
  (if (or (pixhex-d? pixel) (pixbit-d? pixel) (pixrgb-d? pixel))
      (car pixel)
      null))

(define (getPosY pixel)
  (if (or (pixhex-d? pixel) (pixbit-d? pixel) (pixrgb-d? pixel))
      (cadr pixel)
      null))

(define (getColor pixel)
  (if (or (pixhex-d? pixel) (pixbit-d? pixel) (pixrgb-d? pixel))
      (caddr pixel)
      null))

(define (getDepth pixel)
  (if (or (pixhex-d? pixel) (pixbit-d? pixel) (pixrgb-d? pixel))
      (cadddr pixel)
      null))

; modificadores

(define (setPosX pixel x)
  (cond
    [(pixbit-d? pixel) (pixbit-d x (getPosY pixel) (getColor pixel) (getDepth pixel))]
    [(pixrgb-d? pixel) (pixrgb-d x (getPosY pixel) (getColor pixel) (getDepth pixel))]
    [(pixhex-d? pixel) (pixhex-d x (getPosY pixel) (getColor pixel) (getDepth pixel))]
    [else pixel]))

(define (setPosY pixel y)
  (cond
    [(pixbit-d? pixel) (pixbit-d (getPosX pixel) y (getColor pixel) (getDepth pixel))]
    [(pixrgb-d? pixel) (pixrgb-d (getPosX pixel) y (getColor pixel) (getDepth pixel))]
    [(pixhex-d? pixel) (pixhex-d (getPosX pixel) y (getColor pixel) (getDepth pixel))]
    [else pixel]))

;exportacion de funciones para su posterior uso

(provide (all-defined-out))