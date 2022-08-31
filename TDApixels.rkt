#lang racket

; constructores

(define (pixbit-d x y bit d) (list x y bit d))

(define (pixrgb-d x y r g b d) (list x y r g b d))

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
          (and (>= (caddr pixel) 0) (<= (caddr pixel) 255) (>= (cadddr pixel) 0) (<= (cadddr pixel) 255) (>= (car (cddddr pixel)) 0) (<= (car (cddddr pixel)) 255))
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

(define (getPos pixel)
  (if (or (pixhex-d? pixel) (pixbit-d? pixel) (pixrgb-d? pixel))
      (list (car pixel) (cadr pixel))
      null))

(define (getBit pixel)
  (if (pixbit-d? pixel)
      (caddr pixel)
      null))

(define (getRgb pixel)
  (if (pixrgb-d? pixel)
      (list (caddr pixel) (cadddr pixel) (car(cddddr pixel)))
      null))

(define (getHex pixel)
  (if (pixbit-d? pixel)
      (caddr pixel)
      null))

(define (getDepth pixel)
  (if (or (pixhex-d? pixel) (pixbit-d? pixel))
      (cadddr pixel)
      (if (pixrgb-d? pixel)
          (cadr (cddddr pixel))
          null)))

;exportacion de funciones para su posterior uso

(provide (all-defined-out))