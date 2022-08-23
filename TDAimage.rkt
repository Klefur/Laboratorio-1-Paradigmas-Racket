#lang racket

(require "TDApixels.rkt")

(define (image x y . p)
  (if (< (length p) (* x y))
      p
      null))

(define (pixbit-d x y bit d) (list x y bit d))

(display (image 2 2 (pixbit-d 0 0 1 10)(pixbit-d 0 1 1 10)(pixbit-d 1 0 1 10)(pixbit-d 1 1 1 10)))