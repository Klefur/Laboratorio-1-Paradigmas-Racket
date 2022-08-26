#lang racket

;llamado de tda pixeles

(require "TDAPixels.rkt")

; constructor

(define (image x y . p)
  (if (<= (length p) (* x y))
      (cons x (cons y p))
      null))

; pertenencia

(define (bitmap? imagen)
  (if (= (length (cddr imagen)) (* (car imagen) (cadr imagen)))
      (pixbit-d? (caddr imagen))
      #f))

(define (pixmap? imagen)
  (if (= (length (cddr imagen)) (* (car imagen) (cadr imagen)))
      (pixrgb-d? (caddr imagen))
      #f))

(define (hexmap? imagen)
  (if (= (length (cddr imagen)) (* (car imagen) (cadr imagen)))
      (pixhex-d? (caddr imagen))
      #f))

; otras funciones

(define (flipH imagen)
  (map (lambda (pixel) (- (car imagen) (+ (cadr pixel) 1))) (cddr imagen)))

(define (flipV imagen)
  (map (lambda (pixel) (- (car imagen) (+ (car pixel) 1))) (cddr imagen)))

(define (rgb->hex imagen) (+ (* 16 15) 15))

(define img1 (image 2 2 (pixbit-d 0 0 1 10)(pixbit-d 0 1 1 10)(pixbit-d 1 0 1 10)(pixbit-d 0 1 1 10)))

(define img2 (image 2 2 (pixrgb-d 0 0 5 5 5 10)(pixrgb-d 0 1 5 5 5 10)(pixrgb-d 1 0 5 5 5 10)(pixrgb-d 1 1 5 5 5 10)))

(define img3 (image 4 4
                    (pixbit-d 0 0 1 10)(pixbit-d 0 1 1 10)(pixbit-d 0 2 1 10)(pixbit-d 0 3 1 10)
                    (pixbit-d 1 0 1 10)(pixbit-d 1 1 1 10)(pixbit-d 1 2 1 10)(pixbit-d 1 3 1 10)
                    (pixbit-d 2 0 1 10)(pixbit-d 2 1 1 10)(pixbit-d 2 2 1 10)(pixbit-d 2 3 1 10)
                    (pixbit-d 3 0 1 10)(pixbit-d 3 1 1 10)(pixbit-d 3 2 1 10)(pixbit-d 3 3 1 10)))