#lang racket

;llamado de tda pixeles

(require "TDAPixel.rkt")

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

(define (image? imagen)
  (or (pixmap? imagen) (bitmap? imagen) (hexmap? imagen)))
 
; selectores

(define (getLenX imagen)
  (if (image? imagen)
      (car imagen)
      null))

(define (getLenY imagen)
  (if (image? imagen)
      (cadr imagen)
      null))

(define (getPixeles imagen)
  (if (image? imagen)
      (caddr imagen)
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
  (append imagen p))

(define (setLenX imagen x)
  (if (image? imagen)
      (setPixeles (image x (getLenY imagen)) (getPixeles imagen))
      null))

(define (setLenY imagen y)
  (if (image? imagen)
      (setPixeles (image (getLenX imagen) y) (getPixeles imagen))
      null))

(define (setCompressV imagen . color)
  (append imagen color))

(define (flipH imagen)
  (define (lambda1 pixeles)
     (if (null? pixeles)
         null
         (cons (setPosX (firstPix pixeles) (- (getLenX imagen) (+ (getPosX (firstPix pixeles)) 1))) (lambda1 (nextPix pixeles)))))
  (if (image? imagen)
      (setPixeles imagen (lambda1 (getPixeles imagen)))
      null))

(define (flipV imagen)
  (define (lambda1 pixeles)
     (if (null? pixeles)
         null
         (cons (setPosY (firstPix pixeles) (- (getLenY imagen) (+ (getPosY (firstPix pixeles)) 1))) (lambda1 (nextPix pixeles)))))
  (if (image? imagen)
      (setPixeles imagen (lambda1 (getPixeles imagen)))
      null))
  
(define (crop imagen x0 y0 x1 y1)
  (define (lambda1 pixeles)
     (if (null? pixeles)
         null
         (if (and (<= (getPosX (firstPix pixeles)) x1) (<= (getPosY (firstPix pixeles)) y1)
                  (>= (getPosX (firstPix pixeles)) x0) (>= (getPosY (firstPix pixeles)) y0))
             (cons (setPosY (setPosX (firstPix pixeles) (- (getPosX (firstPix pixeles)) x0)) (- (getPosY (firstPix pixeles)) y0)) (lambda1 (nextPix pixeles)))
             (lambda1 (nextPix pixeles)))))
  (if (and (<= 0 x0) (>= (getLenX imagen) x1) (<= 0 y0) (>= (getLenX imagen) y1))
      (append (image (+ (- x1 x0) 1) (+ (- y1 y0) 1)) (lambda1 (getPixeles imagen)))
      imagen))

(define (imgRGB->imgHex imagen)
  (if (pixmap? imagen)
      (setPixeles imagen (map pixrgb->pixhex (getPixeles imagen)))
      imagen))

(define (histogram imagen)
  (define (histogramR pixeles histo)
    (if (null? pixeles)
        histo
        (histogramR (nextPix pixeles) (agregar (car pixeles) histo))))
  (if (image? imagen)
      (histogramR (getPixeles imagen) (list))
      imagen))

(define (rotate90 imagen)
  (if (image? imagen)
      (setPixeles (image (getLenY imagen) (getLenX imagen)) (map (lambda (p) (setPosX (setPosY p (getPosX p)) (- (- (getLenY imagen) 1) (getPosY p)))) (getPixeles imagen)))
      null))

(define (compress imagen)
  (setCompressV (setPixeles (image (getLenX imagen) (getLenY imagen))
  (filter (lambda (pixel) (not (equal? (car (getMayor (histogram imagen))) (getColor pixel)))) (getPixeles imagen)))
  (car (getMayor (histogram imagen)))))

; otras funciones

(define (getMayor histograma)
  (define (mRepeat histograma mayor)
    (if (null? histograma)
        mayor
        (if (> (cadr (car histograma)) (cadr mayor))
            (mRepeat (cdr histograma) (car histograma))
            (mRepeat (cdr histograma) mayor))))
  (mRepeat (cdr histograma) (car histograma)))

(define (esta? pixel histograma)
  (if (null? histograma)
      #f
      (if (equal? (getColor pixel) (car (car histograma)))
          #t
          (esta? pixel (cdr histograma)))))

(define (agregar pixel histograma)
  (if (esta? pixel histograma)
      (agregar1 (getColor pixel) histograma)
      (append histograma (list (list (getColor pixel) 1)))))

(define (agregar1 color histograma)
  (if (null? histograma)
      null
      (if (equal? color (car (car histograma)))
      (cons (list (car (car histograma)) (+ (cadr (car histograma)) 1)) (agregar1 color (cdr histograma)))
      (cons (car histograma) (agregar1 color (cdr histograma))))))
  

; img prueba

(define img1 (image 2 2 (pixbit-d 0 0 1 10)(pixbit-d 1 0 0 10)(pixbit-d 0 1 1 10)(pixbit-d 1 1 1 10)))

(define img2 (image 2 2 (pixrgb-d 0 0 55 205 105 10)(pixrgb-d 1 0 5 5 5 10)
                    (pixrgb-d 0 1 5 5 5 10)(pixrgb-d 1 1 5 5 5 10)))

(define img3 (image 4 4
                    (pixbit-d 0 0 1 10)(pixbit-d 1 0 0 10)(pixbit-d 2 0 1 10)(pixbit-d 3 0 1 10)
                    (pixbit-d 0 1 1 10)(pixbit-d 1 1 1 10)(pixbit-d 2 1 1 10)(pixbit-d 3 1 1 10)
                    (pixbit-d 0 2 1 10)(pixbit-d 1 2 1 10)(pixbit-d 2 2 1 10)(pixbit-d 3 2 1 10)
                    (pixbit-d 0 3 1 10)(pixbit-d 1 3 1 10)(pixbit-d 2 3 1 10)(pixbit-d 3 3 1 10)))

(define img4 (image 3 2
                    (pixbit-d 0 0 1 10)(pixbit-d 1 0 1 10)(pixbit-d 2 0 1 10)
                    (pixbit-d 0 1 1 10)(pixbit-d 1 1 1 10)(pixbit-d 2 1 1 10)))

(define prueba (getPixeles img3))
