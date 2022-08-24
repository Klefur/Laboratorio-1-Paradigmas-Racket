#lang racket

;llamado de tda pixeles

(require "TDApixels.rkt")

; constructor

(define (image x y . p)
  (if (<= (length p) (* x y))
      (cons x (cons y p))
      null))

; pertenencia

(define (bitmap? imagen)
  (if (= (length (cddr imagen)) (* (car imagen) (cadr imagen)))
      (if (map pixbit-d? (caddr imagen))
          (map pixbit-d? (caddr imagen))
          #f)
      #f))

(define (pixmap? imagen)
  (if (= (length (cddr imagen)) (* (car imagen) (cadr imagen)))
      (if (map pixrgb-d? (caddr imagen))
          (caddr imagen)
          #f)
      #f))

(define (hexmap? imagen)
  (if (= (length (cddr imagen)) (* (car imagen) (cadr imagen)))
      (if (pixhex-d? (caddr imagen))
          (caddr imagen)
          #f)
      #f))

(bitmap? (image 2 2 (pixbit-d 0 0 1 10)(pixbit-d 0 1 1 10)(pixbit-d 1 0 1 10)(pixbit-d 1 1 1 10)))
(bitmap? (image 2 2 (pixbit-d 0 0 5 10)(pixbit-d 0 1 5 10)(pixbit-d 1 0 5 10)(pixbit-d 1 1 5 10)))