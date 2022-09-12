#lang racket
;-----------------------------------TDA PIXEL-------------------------------------------------------------------

;-----------------------------------REPRESENTACION--------------------------------------------------------------

; Este TDA corresponde a un pixel y dada su similitud los selectores y modificadores seran iguales en su mayoria.
; Se guarda en una lista la posicion en x, y, su color y su profundidad
; pixrgb (int X int X lista X int)
; pixbit (int X int X int X int)
; pixhex (int X int X string X int)
;-----------------------------------CONSTRUCTORES---------------------------------------------------------------

;Dom: int X int X int X int
;Rec: lista del tipo pixbit-d
;Descripcion: Se crea un pixel con color bit
(define (pixbit-d x y bit d) (list x y bit d))

;Dom: int X int X int X int X int X int
;Rec: lista del tipo pixrgb-d
;     (int X int X lista X int)
;Descripcion: Se crea un pixel con color RGB
(define (pixrgb-d x y r g b d) (list x y (list r g b) d))

;Dom: int X int X string X int
;Rec: lista del tipo pixhex-d
;Descripcion: Se crea un pixel con color hexagesimal
(define (pixhex-d x y hex d) (list x y hex d))

;-----------------------------------PERTENENCIA----------------------------------------------------------------

;Dom: lista del tipo pixel
;Rec: Boolean
;Descripcion: Se verifica si el pixel es pixbit-d
(define (pixbit-d? pixel)
  (if (list? pixel)
      (if (= (length pixel) 4)
          (if (number? (caddr pixel))
              (or (= (caddr pixel) 1) (= (caddr pixel) 0))
              #f)
          #f)
      #f))

;Dom: lista del tipo pixel
;Rec: Boolean
;Descripcion: Se verifica si el pixel es pixrgb-d
(define (pixrgb-d? pixel)
  (if (list? pixel)
      (if (= (length pixel) 4)
          (if (list? (caddr pixel))
              (andmap (lambda (c) (and (>= c 0) (<= c 255))) (caddr pixel))
              #f)
          #f)
      #f))

;Dom: lista del tipo pixel
;Rec: Boolean
;Descripcion: Se verifica si el pixel es pixhex-d
(define (pixhex-d? pixel)
  (if (list? pixel)
      (if (= (length pixel) 4)
          (if (string? (caddr pixel))
              (and (equal? (string-ref (caddr pixel) 0) #\#) (= (string-length (caddr pixel)) 7))
              #f)
          #f)
      #f))

;-----------------------------------SELECTORES-----------------------------------------------------------------

;Dom: lista del tipo pixel [bit | hex | rgb]
;Rec: int
;Descripcion: entrega la posicion en x del pixel
(define (getPosX pixel)
  (if (or (pixhex-d? pixel) (pixbit-d? pixel) (pixrgb-d? pixel))
      (car pixel)
      null))

;Dom: lista del tipo pixel [bit | hex | rgb]
;Rec: int
;Descripcion: entrega la posicion en y del pixel
(define (getPosY pixel)
  (if (or (pixhex-d? pixel) (pixbit-d? pixel) (pixrgb-d? pixel))
      (cadr pixel)
      null))

;Dom: lista del tipo pixel [bit | hex | rgb]
;Rec: [int | string | lista]
;Descripcion: entrega el color del pixel
(define (getColor pixel)
  (if (or (pixhex-d? pixel) (pixbit-d? pixel) (pixrgb-d? pixel))
      (caddr pixel)
      null))

;Dom: lista del tipo pixel [bit | hex | rgb]
;Rec: int
;Descripcion: entrega la profundidad del pixel
(define (getDepth pixel)
  (if (or (pixhex-d? pixel) (pixbit-d? pixel) (pixrgb-d? pixel))
      (cadddr pixel)
      null))

;Dom: lista del tipo pixrgb-d 
;Rec: int
;Descripcion: entrega el color rojo de un pixrgb-d
(define (getR pixel)
  (if (pixrgb-d? pixel)
      (car (getColor pixel))
      null))

;Dom: lista del tipo pixrgb-d
;Rec: int
;Descripcion: entrega el color verde de un pixrgb-d
(define (getG pixel)
  (if (pixrgb-d? pixel)
      (cadr (getColor pixel))
      null))

;Dom: lista del tipo pixrgb-d
;Rec: int
;Descripcion: entrega el color azul de un pixrgb-d
(define (getB pixel)
  (if (pixrgb-d? pixel)
      (caddr (getColor pixel))
      null))

;-----------------------------------MODIFICADORES--------------------------------------------------------------

;Dom: lista del tipo pixel [bit | hex | rgb]
;Rec: lista del tipo pixel [bit | hex | rgb]
;Descripcion: modifica la posicion en x del pixel
(define (setPosX pixel x)
  (cond
    [(pixbit-d? pixel) (pixbit-d x (getPosY pixel) (getColor pixel) (getDepth pixel))]
    [(pixrgb-d? pixel) (pixrgb-d x (getPosY pixel) (car (getColor pixel)) (cadr (getColor pixel)) (caddr (getColor pixel)) (getDepth pixel))]
    [(pixhex-d? pixel) (pixhex-d x (getPosY pixel) (getColor pixel) (getDepth pixel))]
    [else null]))

;Dom: lista del tipo pixel [bit | hex | rgb]
;Rec: lista del tipo pixel [bit | hex | rgb]
;Descripcion: modifica la posicion en y del pixel
(define (setPosY pixel y)
  (cond
    [(pixbit-d? pixel) (pixbit-d (getPosX pixel) y (getColor pixel) (getDepth pixel))]
    [(pixrgb-d? pixel) (pixrgb-d (getPosX pixel) y (car (getColor pixel)) (cadr (getColor pixel)) (caddr (getColor pixel)) (getDepth pixel))]
    [(pixhex-d? pixel) (pixhex-d (getPosX pixel) y (getColor pixel) (getDepth pixel))]
    [else null]))

;Dom: lista del tipo pixbit-d
;Rec: lista del tipo pixbit-d
;Descripcion: modifica el color de un pixrgb-d
(define (setBit pixel bit)
  (if (and (pixbit-d? pixel) (or (= bit 1) (= bit 0)))
      (pixbit-d (getPosX pixel) bit (getDepth pixel))
      pixel))

;Dom: lista del tipo pixrgb-d
;Rec: lista del tipo pixrgb-d
;Descripcion: modifica el color rojo de un pixrgb-d
(define (setR pixel R)
  (if (and (pixrgb-d? pixel) (or (<= R 255) (>= R 0)))
      (pixrgb-d (getPosX pixel) (getPosY pixel) R (getG pixel) (getB pixel) (getDepth pixel))
      pixel))

;Dom: lista del tipo pixrgb-d
;Rec: lista del tipo pixrgb-d
;Descripcion: modifica el color verde de un pixrgb-d
(define (setG pixel G)
  (if (and (pixrgb-d? pixel) (or (<= G 255) (>= G 0)))
      (pixrgb-d (getPosX pixel) (getPosY pixel) (getR pixel) G (getB pixel) (getDepth pixel))
      pixel))

;Dom: lista del tipo pixrgb-d
;Rec: lista del tipo pixrgb-d
;Descripcion: modifica el color azul de un pixrgb-d
(define (setB pixel B)
  (if (and (pixrgb-d? pixel) (or (<= B 255) (>= B 0)))
      (pixrgb-d (getPosX pixel) (getPosY pixel) (getR pixel) (getG pixel) B (getDepth pixel))
      pixel))

;Dom: lista del tipo pixhex-d
;Rec: lista del tipo pixhex-d
;Descripcion: modifica el color de un pixhex-d
(define (setHex pixel hex)
  (if (pixhex-d? pixel)
      (pixhex-d (getPosX pixel) (getPosY pixel) hex (getDepth pixel))
      pixel))

;-----------------------------------OTRAS FUNCIONES------------------------------------------------------------

;Dom: lista del tipo pixrgb-d
;Rec: lista del tipo pixhex-d
;Descripcion: convierte un pixrgb-d a pixhex-d
(define (pixrgb->pixhex pixel)
  (if (pixrgb-d? pixel)
      (pixhex-d (getPosX pixel) (getPosY pixel) (string-append "#" (car (rgb->hex pixel)) (cadr (rgb->hex pixel)) (caddr (rgb->hex pixel))) (getDepth pixel))
      null))

;Dom: int
;Rec: string
;Descripcion: transforma un entero a formato hexagesimal
(define (rgb->stringHex numero)
  (cond
    [(= numero 10) "A"]
    [(= numero 11) "B"]
    [(= numero 12) "C"]
    [(= numero 13) "D"]
    [(= numero 14) "E"]
    [(= numero 15) "F"]
    [else (number->string numero)]))

;Dom: lista del pixrgb-d
;Rec: string
;Descripcion: transforma el color rgb a color hex
(define (rgb->hex pixel)
  (map (lambda (c)
         (if (= (remainder c 16) 0)
             (string-append (rgb->stringHex (/ (- c (remainder c 16)) 16)) "0")
             (string-append (rgb->stringHex (/ (- c (remainder c 16)) 16)) (rgb->stringHex (remainder c 16)))))
       (getColor pixel)))

;Dom: lista del pixbit-d
;Rec: string
;Descripcion: transforma un pixel bit a string
(define (pixbit->string pixel)
  (if (pixbit-d? pixel)
      (if (= (getColor pixel) 1)
          "#FFFFFF"
          "#000000")
      " "))

;Dom: lista del pixrgb-d
;Rec: string
;Descripcion: transforma un pixel rgb a string
(define (pixrgb->string pixel)
  (if (pixrgb-d? pixel)
      (getColor (pixrgb->pixhex pixel))
      " "))

;Dom: lista del pixhex-d
;Rec: string
;Descripcion: transforma un pixel hex a string
(define (pixhex->string pixel)
  (if (pixhex-d? pixel)
      (getColor pixel)
      " "))

;exportacion de funciones para su posterior uso

(provide (all-defined-out))