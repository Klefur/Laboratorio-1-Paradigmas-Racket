#lang racket

;llamado de tda pixeles

(require "TDAPixels.rkt")

; constructor

(define (image x y . p)
  (if (<= (length p) (* x y))
      (if (null? p)
          (list x y)
          (list x y p))
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

(define (nextPix pixeles)
  (if (list? pixeles)
      (cdr pixeles)
      null))

; modificadores

(define (setPixeles imagen . p)
  (if (null? p)
      imagen
      (append imagen p)))

; otras funciones

(define (flipH imagen)
  (define (lambda1 pixeles)
     (if (null? pixeles)
         null
         (cons (setPosX (firstPix pixeles) (- (getLenX imagen) (+ (getPosX (firstPix pixeles)) 1))) (lambda1 (nextPix pixeles)))))
  (setPixeles (image (getLenX imagen) (getLenY imagen)) (lambda1 (getPixeles imagen))))

(define (flipV imagen)
  (define (lambda1 pixeles)
     (if (null? pixeles)
         null
         (cons (setPosY (firstPix pixeles) (- (getLenY imagen) (+ (getPosY (firstPix pixeles)) 1))) (lambda1 (nextPix pixeles)))))
  (setPixeles (image (getLenX imagen) (getLenY imagen)) (lambda1 (getPixeles imagen))))

(define (crop imagen x0 y0 x1 y1)
  (define (lambda1 pixeles)
     (if (null? pixeles)
         pixeles
         (if (and (<= (getPosX (firstPix pixeles)) x1) (<= (getPosY (firstPix pixeles)) y1)
                  (>= (getPosX (firstPix pixeles)) x0) (>= (getPosY (firstPix pixeles)) y0))
             (cons (firstPix pixeles) (lambda1 (nextPix pixeles)))
             (lambda1 (nextPix pixeles)))))
  (setPixeles (image (getLenX imagen) (getLenY imagen)) (lambda1 (getPixeles imagen))))

(define (imgRGB->imgHex imagen)
  (if (pixmap? imagen)
      (setPixeles (image (getLenX imagen) (getLenY imagen)) (map pixrgb->pixhex (getPixeles imagen)))
      imagen))

(define img1 (image 2 2 (pixbit-d 0 0 1 10)(pixbit-d 0 1 1 10)(pixbit-d 1 0 1 10)(pixbit-d 1 1 1 10)))

(define img2 (image 2 2 (pixrgb-d 0 0 55 205 105 10)(pixrgb-d 0 1 5 5 5 10)
                        (pixrgb-d 1 0 5 5 5 10)(pixrgb-d 1 1 5 5 5 10)))

(define img3 (image 4 4
                    (pixbit-d 0 0 1 10)(pixbit-d 0 1 1 10)(pixbit-d 0 2 1 10)(pixbit-d 0 3 1 10)
                    (pixbit-d 1 0 1 10)(pixbit-d 1 1 1 10)(pixbit-d 1 2 1 10)(pixbit-d 1 3 1 10)
                    (pixbit-d 2 0 1 10)(pixbit-d 2 1 1 10)(pixbit-d 2 2 1 10)(pixbit-d 2 3 1 10)
                    (pixbit-d 3 0 1 10)(pixbit-d 3 1 1 10)(pixbit-d 3 2 1 10)(pixbit-d 3 3 1 10)))