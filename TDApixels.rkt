#lang racket

; constructores

(define (pixbit-d x y bit d) (list x y bit d))

(define (pixrgb-d x y r g b d) (list x y r g b d))

(define (pixhex-d x y hex d) (list x y hex d))

; pertenencia

(define (pixbit-d? pixel)
  (if (list? pixel)
      (if (= (length pixel) 4)
          (if (or (= (caddr pixel) 1) (= (caddr pixel) 0))
              #t
              #f)
          #f)
      #f))

(define (pixrgb-d? pixel)
  (if (list? pixel)
      (if (= (length pixel) 6)
          (if (and (>= (caddr pixel) 0) (<= (caddr pixel) 255)
               (>= (cadddr pixel) 0) (<= (cadddr pixel) 255)
               (>= (car (cddddr pixel)) 0) (<= (car (cddddr pixel)) 255))
              #t
              #f)
          #f)
      #f))

(define (pixhex-d? pixel)
  (if (list? pixel)
      (if (= (length pixel) 4)
          (if (string? (caddr pixel))
              (if (and (= (car pixel) "#") (= (length pixel) 7))
                  #t
                  #f)
              #f)
          #f)
      #f))

;exportacion de funciones para su posterior uso

(provide (all-defined-out))