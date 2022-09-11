#lang racket

;llamado de tda pixeles

(require "TDAPixel.rkt")
;-----------------------------------TDA IMAGE-------------------------------------------------------------------

;-----------------------------------REPRESENTACION--------------------------------------------------------------

; Este TDA corresponde a una imagen.
; Se guarda en una lista el ancho, el largo, una lista de pixeles [pixbit-d | pixrgb-d | pixhex-d] y un color para
; definir que fue comprimido
; (int X int X [pixbit-d | pixrgb-d | pixhex-d] X color [int | lista | string])
;-----------------------------------CONSTRUCTORES---------------------------------------------------------------

;Dom: ancho x (int), largo y (int), pixeles p (pixbit-d | pixrgb-d | pixhex-d)
;Rec: lista image
;Descripcion: Se crea una imagen
(define (image x y . p)
  (if (<= (length p) (* x y))
      (if (null? p)
          (list x y)
          (list x y p))
      null))

;-----------------------------------PERTENENCIA-----------------------------------------------------------------

;Dom: lista del tipo image
;Rec: Boolean
;Descripcion: Se verifica si la imagen esta compuesta de pixeles del tipo pixbit-d
(define (bitmap? imagen)
  (if (= (length (caddr imagen)) (* (car imagen) (cadr imagen)))
      (pixbit-d? (car (caddr imagen)))
      #f))

;Dom: lista del tipo image
;Rec: Boolean
;Descripcion: Se verifica si la imagen esta compuesta de pixeles del tipo pixrgb-d
(define (pixmap? imagen)
  (if (= (length (caddr imagen)) (* (car imagen) (cadr imagen)))
      (pixrgb-d? (car (caddr imagen)))
      #f))

;Dom: lista del tipo image
;Rec: Boolean
;Descripcion: Se verifica si la imagen esta compuesta de pixeles del tipo pixhex-d
(define (hexmap? imagen)
  (if (= (length (caddr imagen)) (* (car imagen) (cadr imagen)))
      (pixhex-d? (car (caddr imagen)))
      #f))

;Dom: lista del tipo image
;Rec: Boolean
;Descripcion: Se verifica si la imagen esta comprimida
(define (compressed? imagen)
  (= (length (imagen)) 4))

;Dom: lista del tipo image
;Rec: Boolean
;Descripcion: Se verifica si la imagen tiene un formato de pixeles valido
(define (image? imagen)
  (or (pixmap? imagen) (bitmap? imagen) (hexmap? imagen)))
 
;-----------------------------------SELECTORES-----------------------------------------------------------------

;Dom: lista del tipo image
;Rec: int
;Descripcion: entrega el ancho de la imagen
(define (getLenX imagen)
  (if (image? imagen)
      (car imagen)
      null))

;Dom: lista del tipo image
;Rec: int
;Descripcion: entrega el largo de la imagen
(define (getLenY imagen)
  (if (image? imagen)
      (cadr imagen)
      null))

;Dom: lista del tipo image
;Rec: lista [pixbit-d | pixrgb-d | pixhex-d]
;Descripcion: entrega los pixeles que componen la imagen
(define (getPixeles imagen)
  (if (image? imagen)
      (caddr imagen)
      null))

;Dom: lista de pixeles
;Rec: [pixbit-d | pixrgb-d | pixhex-d]
;Descripcion: entrega el primer pixel de una lista de pixeles
(define (firstPix pixeles)
  (if (list? pixeles)
      (car pixeles)
      null))
;Dom: lista de pixeles
;Rec: [pixbit-d | pixrgb-d | pixhex-d]
;Descripcion: entrega el resto pixeles de una lista de pixeles
(define (nextPix pixeles)
  (if (list? pixeles)
      (cdr pixeles)
      null))

;-----------------------------------MODIFICADORES--------------------------------------------------------------

;Dom: lista del tipo image
;Rec: lista del tipo image
;Descripcion: modifica los pixeles de una imagen
(define (setPixeles imagen . p)
  (if (= (length imagen) 2)
      (append imagen p)
      (append (image (getLenX imagen) (getLenY imagen)) p)))

;Dom: lista del tipo image
;Rec: lista del tipo image
;Descripcion: modifica el ancho de una imagen
(define (setLenX imagen x)
  (if (image? imagen)
      (setPixeles (image x (getLenY imagen)) (getPixeles imagen))
      null))

;Dom: lista del tipo image
;Rec: lista del tipo image
;Descripcion: modifica el largo de una imagen
(define (setLenY imagen y)
  (if (image? imagen)
      (setPixeles (image (getLenX imagen) y) (getPixeles imagen))
      null))

;Dom: lista del tipo image
;Rec: lista del tipo image
;Descripcion: establece el valor del color que fue comprimido
(define (setCompressV imagen . color)
  (append imagen color))

;-----------------------------------OTRAS FUNCIONES------------------------------------------------------------

;Dom: lista del tipo image
;Rec: lista del tipo image
;Descripcion: modifica los pixeles de una imagen volteandola horizontalmente
;Recursion: natural
;Justificacion: permite ir recorrer la lista de pixeles y a su vez ir modificandolos para contruir la imagen volteada
(define (flipH imagen)
  (define (lambda1 pixeles)
     (if (null? pixeles)
         null
         (cons (setPosX (firstPix pixeles) (- (getLenX imagen) (+ (getPosX (firstPix pixeles)) 1))) (lambda1 (nextPix pixeles)))))
  (if (image? imagen)
      (setPixeles imagen (lambda1 (getPixeles imagen)))
      null))

;Dom: lista del tipo image
;Rec: lista del tipo image
;Descripcion: modifica los pixeles de una imagen volteandola verticalmente
;Recursion: natural
;Justificacion: permite ir recorrer la lista de pixeles y a su vez ir modificandolos para contruir la imagen volteada
(define (flipV imagen)
  (define (lambda1 pixeles)
     (if (null? pixeles)
         null
         (cons (setPosY (firstPix pixeles) (- (getLenY imagen) (+ (getPosY (firstPix pixeles)) 1))) (lambda1 (nextPix pixeles)))))
  (if (image? imagen)
      (setPixeles imagen (lambda1 (getPixeles imagen)))
      null))

;Dom: image X int X int X int X int
;Rec: lista del tipo image
;Descripcion: modifica los pixeles de una imagen recortandola de acuerdo a parametros establecidos, reescalando la imagen
;en tamaño y cambiando la posicion de los pixeles
;Recursion: natural
;Justificacion: permite ir recorrer la lista de pixeles y a su vez ir modificandolos para contruir la imagen recortada
(define (crop imagen x0 y0 x1 y1)
  (define (lambda1 pixeles)
     (if (null? pixeles)
         null
         (if (and (<= (getPosX (firstPix pixeles)) x1) (<= (getPosY (firstPix pixeles)) y1)
                  (>= (getPosX (firstPix pixeles)) x0) (>= (getPosY (firstPix pixeles)) y0))
             (cons (setPosY (setPosX (firstPix pixeles) (- (getPosX (firstPix pixeles)) x0)) (- (getPosY (firstPix pixeles)) y0)) (lambda1 (nextPix pixeles)))
             (lambda1 (nextPix pixeles)))))
  (if (and (<= 0 x0) (>= (getLenX imagen) x1) (<= 0 y0) (>= (getLenX imagen) y1))
      (setPixeles (image (+ (- x1 x0) 1) (+ (- y1 y0) 1)) (lambda1 (getPixeles imagen)))
      imagen))

;Dom: lista del tipo image con pixeles del tipo pixrgb-d
;Rec: lista del tipo image con pixeles del tipo pixhex-d
;Descripcion: transforma una imagen pixrgb-d a una imagen pixhex-d
(define (imgRGB->imgHex imagen)
  (if (pixmap? imagen)
      (setPixeles imagen (map pixrgb->pixhex (getPixeles imagen)))
      imagen))

;Dom: lista del tipo image
;Rec: lista
;Descripcion: crea una lista de los colores y la cantidad de veces que se repite en la imagen
;Recursion: cola
;Justificacion: facilita recorrer la lista de pixeles y a su vez cuando este vacia entregar directamente el histograma
(define (histogram imagen)
  (define (histogramR pixeles histo)
    (if (null? pixeles)
        histo
        (histogramR (nextPix pixeles) (agregar (car pixeles) histo))))
  (if (image? imagen)
      (histogramR (getPixeles imagen) (list))
      imagen))

;Dom: lista del tipo image
;Rec: lista del tipo image
;Descripcion: rota una imagen en 90° en sentido horario
(define (rotate90 imagen)
  (if (image? imagen)
      (setPixeles imagen (map (lambda (p) (setPosX (setPosY p (getPosX p)) (- (- (getLenY imagen) 1) (getPosY p)))) (getPixeles imagen)))
      null))

;Dom: lista del tipo image
;Rec: lista del tipo image
;Descripcion: comprime una imagen eliminando los pixeles mas repetidos
(define (compress imagen)
  (setCompressV (setPixeles (image (getLenX imagen) (getLenY imagen))
  (filter (lambda (pixel) (not (equal? (car (getMayor (histogram imagen))) (getColor pixel)))) (getPixeles imagen)))
  (car (getMayor (histogram imagen)))))

;Dom: lista del tipo image
;Rec: lista del tipo image
;Descripcion: se le entrega un funcion f para modificar una imagen
(define (edit f imagen)
  (setPixeles imagen (map f (getPixeles imagen))))

;Dom: lista del tipo pixbit
;Rec: lista del tipo pixbit
;Descripcion: invierte los colores del pixel
(define (invertColorBit pixbit)
  (if (pixbit-d? pixbit)
      (pixbit-d (getPosX pixbit) (getPosY pixbit) (- 1 (getColor pixbit)) (getDepth pixbit))
      null))

;Dom: lista del tipo pixrgb
;Rec: lista del tipo pixrgb
;Descripcion: invierte los colores del pixel
(define (invertColorRGB pixrgb)
  (if (pixrgb-d? pixrgb)
      (pixrgb-d (getPosX pixrgb) (getPosY pixrgb) (- 255 (getR pixrgb)) (- 255 (getG pixrgb)) (- 255 (getB pixrgb)) (getDepth pixrgb))
      null))

;Dom: funcion x funcion x funcion x pixrgb
;Rec: pixrgb
;Descripcion: se elije un canal del pixel y se le introduce una funcion de operacion para ese canal modificando el color del pixel
(define (adjustChannel f1 f2 f3) (lambda (p)
  (f2 p (f3 (f1 p)))))

;Dom: lista
;Rec: lista
;Descripcion: entrega el color mas repetido de la lista histograma
;Recursion: cola
;Justificacion: facilita el seguimiento del mayor entregandolo por parametro retornando devuelta cuando ya se haya recorrido todo el histograma 
(define (getMayor histograma)
  (define (getMR histograma mayor)
    (if (null? histograma)
        mayor
        (if (> (cadr (car histograma)) (cadr mayor))
            (mRepeat (cdr histograma) (car histograma))
            (mRepeat (cdr histograma) mayor))))
  (getMR (cdr histograma) (car histograma)))

;Dom: lista
;Rec: boolean
;Descripcion: entrega un booleando que indica si esta un color dentro de la lista histograma
;Recursion: cola
;Justificacion: al no necesitar estados pendientes se recorre la lista hasta encontrar el color o caso contrario retornar falso
(define (estaC? pixel histograma)
  (if (null? histograma)
      #f
      (if (equal? (getColor pixel) (car (car histograma)))
          #t
          (estaC? pixel (cdr histograma)))))

;Dom: lista
;Rec: lista
;Descripcion: la funcion agrega el color a la lista histograma en caso de no estar y si esta se agrega uno a donde corresponde
(define (agregar pixel histograma)
  (if (estaC? pixel histograma)
      (agregar1 (getColor pixel) histograma)
      (append histograma (list (list (getColor pixel) 1)))))

;Dom: lista
;Rec: lista
;Descripcion: agrega 1 al color que corresponde
;Recursion: natural
;Justificacion: facilita la modificacion directa de alguno de los elementos de la lista ya que se trabaja uno a uno
(define (agregar1 color histograma)
  (if (null? histograma)
      null
      (if (equal? color (car (car histograma)))
      (cons (list (car (car histograma)) (+ (cadr (car histograma)) 1)) (agregar1 color (cdr histograma)))
      (cons (car histograma) (agregar1 color (cdr histograma))))))

;Dom: int
;Rec: int
;Descripcion: agrega 1
(define (incCh c) (+ c 1))

; img prueba

(define img1 (image 2 2 (pixbit-d 0 0 1 20)(pixbit-d 1 0 0 10)(pixbit-d 0 1 1 20)(pixbit-d 1 1 1 10)))

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
