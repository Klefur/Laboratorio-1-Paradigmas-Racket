#lang racket

;llamado de tda pixeles

(require "TDAPixels.rkt")

; constructor

(define (image x y p)
  (if (<= (length p) (* x y))
      (list x y p)
      null))

; pertenencia

(define (bitmap? imagen)
  (if (= (length (caddr imagen)) (* (car imagen) (cadr imagen)))
      (pixbit-d? (car (caddr imagen)))
      #f))

(define (pixmap? imagen)
  (if (= (length (caddr imagen)) (* (car imagen) (cadr imagen)))
      (pixrgb-d? (car (caddr imagen)))
      #f))

(define (hexmap? imagen)
  (if (= (length (caddr imagen)) (* (car imagen) (cadr imagen)))
      (pixhex-d? (car (caddr imagen)))
      #f))

(define (compressed? imagen)
  (if (= (length (caddr imagen)) (* (car imagen) (cadr imagen)))
      #f
      #t))
 
; selectores

(define (getLenX imagen)
  (if (or (pixmap? imagen) (bitmap? imagen) (hexmap? imagen))
      (car imagen)
      null))

(define (getLenY imagen)
  (if (or (pixmap? imagen) (bitmap? imagen) (hexmap? imagen))
      (cadr imagen)
      null))

(define (getPixeles imagen)
  (if (or (bitmap? imagen) (hexmap? imagen) (pixmap? imagen))
      (caddr imagen)
      null))

(define (getDepth imagen)
  (if (or (bitmap? imagen) (hexmap? imagen) (pixmap? imagen))
      (cadddr imagen)
      null))

(define (firstPix pixeles)
  (if (list? pixeles)
      (car pixeles)
      null))

; otras funciones

(define (flipH imagen)
  (define (lambda1 pixeles)
     (if (null? pixeles)
         null
         (cons (setPosX (firstPix pixeles) (- (getLenX imagen) (+ (getPosX (firstPix pixeles)) 1))) (lambda1 (cdr pixeles)))))
  (image (getLenX imagen) (getLenY imagen) (lambda1 (getPixeles imagen))))

(define (flipV imagen)
  (define (lambda1 pixeles)
     (if (null? pixeles)
         null
         (cons (setPosY (firstPix pixeles) (- (getLenY imagen) (+ (getPosY (firstPix pixeles)) 1))) (lambda1 (cdr pixeles)))))
  (image (getLenX imagen) (getLenY imagen) (lambda1 (getPixeles imagen))))


(define (rgb->hex imagen) (+ (* 16 15) 15))

(define img1 (image 2 2 (list (pixbit-d 0 0 1 10)(pixbit-d 0 1 1 10)(pixbit-d 1 0 1 10)(pixbit-d 0 1 1 10))))

(define img2 (image 2 2 (list (pixrgb-d 0 0 5 5 5 10)(pixrgb-d 0 1 5 5 5 10)(pixrgb-d 1 0 5 5 5 10)(pixrgb-d 1 1 5 5 5 10))))

(define img3 (image 4 4
                    (list (pixbit-d 0 0 1 10)(pixbit-d 0 1 1 10)(pixbit-d 0 2 1 10)(pixbit-d 0 3 1 10)
                    (pixbit-d 1 0 1 10)(pixbit-d 1 1 1 10)(pixbit-d 1 2 1 10)(pixbit-d 1 3 1 10)
                    (pixbit-d 2 0 1 10)(pixbit-d 2 1 1 10)(pixbit-d 2 2 1 10)(pixbit-d 2 3 1 10)
                    (pixbit-d 3 0 1 10)(pixbit-d 3 1 1 10)(pixbit-d 3 2 1 10)(pixbit-d 3 3 1 10))))