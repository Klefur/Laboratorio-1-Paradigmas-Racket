#lang racket

(require "TDAPixel.rkt")

; constructores

(define histogram
  (list (list 1 100) (list 0 150)))

; pertenencia

(define (histogram? histograma)
  (and (list? histograma) (andmap list? histograma)))

; selectores

(define (getColorH histograma)
  (if (histogram? histograma)
      (car (car histograma))
      null))

; modificadores

; otras funciones

(define (color? pixel . color)
  (cond
    [(pixbit-d? pixel) (equal? (getColor pixel) (car color))]
    [(pixhex-d? pixel) (equal? (getColor pixel) (car color))]
    [(pixrgb-d? pixel) (equal? (getColor pixel) color)]
    [else null]))



;exportacion de funciones para su posterior uso

(provide (all-defined-out))