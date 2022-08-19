#lang racket

(define (bit x)
  (if (> x 0) 1 0))

(define(pixbit-d x y bit d) (list x y bit d))

(define (pixrgb-d x y r g b d) (list x y r g b d))