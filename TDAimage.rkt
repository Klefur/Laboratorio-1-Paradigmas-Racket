#lang racket

;llamado de tda pixeles

(require "TDAPixels.rkt")

; constructor

(define (image x y . p)
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
(define (getPixeles imagen)
  (if (or (bitmap? imagen) (hexmap? imagen) (pixmap? imagen))
      (caddr imagen)
      null))

; otras funciones

(define (flipH imagen)
  (map (lambda (pixel) (- (car imagen) (+ (cadr pixel) 1))) (cddr imagen)))

(define (flipV imagen)
  (map (lambda (pixel) (- (car imagen) (+ (car pixel) 1))) (cddr imagen)))

(define (aux imagen)
  (define (gen pixeles pixelesNew)
    (if (empty? pixeles)
        pixelesNew
        (if (pixbit-d? (car pixeles))
            (gen (rest pixeles) (append pixelesNew (list (pixbit-d (car (getPos (car pixeles))) (- (car imagen) (+ (cadr (getPos (car pixeles))) 1))
                                                             (getBit (car pixeles)) (getDepth (car pixeles))))))
            (if (pixrgb-d? (car pixeles))
                (gen (rest pixeles) (append pixelesNew (pixbit-d (car (getPos (car pixeles))) (- (car imagen) (+ (cadr (getPos (car pixeles))) 1))
                                                                 (car (getRgb (car pixeles))) (cadr (getRgb (car pixeles))) (caddr (getRgb (car pixeles)))
                                                                 (getDepth (car pixeles)))))
                (if (pixhex-d? (car pixeles))
                    (gen (rest pixeles) (append pixelesNew (pixbit-d (car (getPos (car pixeles))) (- (car imagen) (+ (cadr (getPos (car pixeles))) 1))
                                                                     (getHex (car pixeles)) (getDepth (car pixeles)))))
                    null)))))
  (image (car imagen) (cadr imagen) (gen (getPixeles imagen) (list))))

(define (gen pixeles pixelesNew)
  (if (empty? pixeles)
      pixelesNew
      (gen (rest pixeles) (append pixelesNew (list (pixbit-d (car (getPos (car pixeles))) (- 2 (+ (cadr (getPos (car pixeles))) 1))
                                                             (getBit (car pixeles)) (getDepth (car pixeles))))))))

(define (rgb->hex imagen) (+ (* 16 15) 15))

(define img1 (image 2 2 (pixbit-d 0 0 1 10)(pixbit-d 0 1 1 10)(pixbit-d 1 0 1 10)(pixbit-d 0 1 1 10)))

(define img2 (image 2 2 (pixrgb-d 0 0 5 5 5 10)(pixrgb-d 0 1 5 5 5 10)(pixrgb-d 1 0 5 5 5 10)(pixrgb-d 1 1 5 5 5 10)))

(define img3 (image 4 4
                    (pixbit-d 0 0 1 10)(pixbit-d 0 1 1 10)(pixbit-d 0 2 1 10)(pixbit-d 0 3 1 10)
                    (pixbit-d 1 0 1 10)(pixbit-d 1 1 1 10)(pixbit-d 1 2 1 10)(pixbit-d 1 3 1 10)
                    (pixbit-d 2 0 1 10)(pixbit-d 2 1 1 10)(pixbit-d 2 2 1 10)(pixbit-d 2 3 1 10)
                    (pixbit-d 3 0 1 10)(pixbit-d 3 1 1 10)(pixbit-d 3 2 1 10)(pixbit-d 3 3 1 10)))