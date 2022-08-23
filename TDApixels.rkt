#lang racket
  
(define (pixbit-d x y bit d) (list x y bit d))

(define (pixrgb-d x y r g b d) (list x y r g b d))

(define (pixhex-d x y hex d) (list x y hex d))