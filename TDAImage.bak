#lang racket

;llamado de tda pixeles

(require "TDAPixel_21272789_SaldiviaMonsalve.rkt")
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
  (if (<= (length (caddr imagen)) (* (car imagen) (cadr imagen)))
      (andmap pixbit-d? (caddr imagen))
      #f))

;Dom: lista del tipo image
;Rec: Boolean
;Descripcion: Se verifica si la imagen esta compuesta de pixeles del tipo pixrgb-d
(define (pixmap? imagen)
  (if (<= (length (caddr imagen)) (* (car imagen) (cadr imagen)))
      (andmap pixrgb-d? (caddr imagen))
      #f))

;Dom: lista del tipo image
;Rec: Boolean
;Descripcion: Se verifica si la imagen esta compuesta de pixeles del tipo pixhex-d
(define (hexmap? imagen)
  (if (<= (length (caddr imagen)) (* (car imagen) (cadr imagen)))
      (andmap pixhex-d? (caddr imagen))
      #f))

;Dom: lista del tipo image
;Rec: Boolean
;Descripcion: Se verifica si la imagen esta comprimida
(define (compressed? imagen) (if (image? imagen) (= (length imagen) 4) #f))

;Dom: lista del tipo image
;Rec: Boolean
;Descripcion: Se verifica si la imagen tiene un formato de pixeles valido
(define (image? imagen) (or (pixmap? imagen) (bitmap? imagen) (hexmap? imagen)))
 
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

;Dom: lista del tipo image
;Rec: [int | lista | hex]
;Descripcion: entrega el valor del color si la imagen esta comprimida
(define (getCompressV imagen)
  (if (= (length imagen) 4)
      (cadddr imagen)
      null))

;Dom: lista de pixeles [pixbit-d | pixrgb-d | pixhex-d]
;Rec: [pixbit-d | pixrgb-d | pixhex-d]
;Descripcion: entrega el primer pixel de una lista de pixeles
(define (firstPix pixeles)
  (if (and (list? pixeles) (not (null? pixeles)))
      (car pixeles)
      null))

;Dom: lista de pixeles [pixbit-d | pixrgb-d | pixhex-d]
;Rec: lista de pixeles [pixbit-d | pixrgb-d | pixhex-d]
;Descripcion: entrega el resto pixeles de una lista de pixeles
(define (nextPix pixeles)
  (if (and (list? pixeles) (not (null? pixeles)))
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
         (cons (setPosY (firstPix pixeles) (- (getLenY imagen) (+ (getPosY (firstPix pixeles)) 1))) (lambda1 (nextPix pixeles)))))
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
         (cons (setPosX (firstPix pixeles) (- (getLenX imagen) (+ (getPosX (firstPix pixeles)) 1))) (lambda1 (nextPix pixeles)))))
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
        (histogramR (nextPix pixeles) (agregar (firstPix pixeles) histo))))
  (if (image? imagen)
      (histogramR (getPixeles imagen) (list))
      imagen))

;Dom: lista del tipo image
;Rec: lista del tipo image
;Descripcion: rota una imagen en 90° en sentido horario
(define (rotate90 imagen)
  (if (image? imagen)
      (setPixeles (image (getLenY imagen) (getLenX imagen)) (map (lambda (p) (setPosX (setPosY p (getPosX p)) (- (- (getLenY imagen) 1) (getPosY p)))) (getPixeles imagen)))
      null))

;Dom: lista del tipo image
;Rec: lista del tipo image
;Descripcion: comprime una imagen eliminando los pixeles mas repetidos
(define (compress imagen)
  (if (compressed? imagen)
      imagen
      (setCompressV (setPixeles (image (getLenX imagen) (getLenY imagen))
                                (filter (lambda (pixel) (not (equal? (car (getMayor (histogram imagen))) (getColor pixel)))) (getPixeles imagen)))
                    (car (getMayor (histogram imagen))))))

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

;Dom: funcion X funcion X funcion X pixrgb
;Rec: pixrgb
;Descripcion: se elije un canal del pixel y se le introduce una funcion de operacion para ese canal modificando el color del pixel
(define (adjustChannel f1 f2 f3) (lambda (p)
  (f2 p (f3 (f1 p)))))

;Dom: image X funcion
;Rec: string
;Descripcion: se transforma la imagen a string para posteriormente mostrarla en consola
(define (image->string imagen f)
  (string-append (formarString imagen "" 0 0 (- (getLenX imagen) 1) (- (getLenY imagen) 1) f) "\n"))

;Dom: lista del tipo imagen
;Rec: lista de listas del tipo imagen
;Descripcion: crea una lista de imagenes donde cada imagen tiene la misma profundidad
(define (depthLayers imagen)
  (define (depthogram pixeles depto)
     (if (null? pixeles)
        depto
        (depthogram (nextPix pixeles) (agregarDepto (firstPix pixeles) depto))))
  (define (seteo imagen color f)
    (setPixeles imagen (rellenarPix (getPixeles imagen) 0 0 (- (getLenX imagen) 1) (- (getLenY imagen) 1) color f (getDepth (firstPix (getPixeles imagen))))))
  (if (image? imagen)
      (cond
        [(bitmap? imagen) (map (lambda (img) (seteo img 1 pixbit-d))
                               (map (lambda (pixs) (setPixeles (image (getLenX imagen) (getLenY imagen)) pixs)) (depthogram (getPixeles imagen) (list))))]
        [(pixmap? imagen) (map (lambda (img) (seteo img (list 255 255 255) pixrgb-d))
                               (map (lambda (pixs) (setPixeles (image (getLenX imagen) (getLenY imagen)) pixs)) (depthogram (getPixeles imagen) (list))))]
        [else (map (lambda (img) (seteo img "#FFFFFF" pixhex-d))
                   (map (lambda (pixs) (setPixeles (image (getLenX imagen) (getLenY imagen)) pixs)) (depthogram (getPixeles imagen) (list))))])
      null))

;Dom: lista del tipo image
;Rec: lista del tipo image
;Descripcion: descomprime la imagen rellenando los pixeles faltantes con el color guardado
(define (decompress imagen)
  (define (seteo imagen f)
    (setPixeles imagen (rellenarPix (getPixeles imagen) 0 0 (- (getLenX imagen) 1) (- (getLenY imagen) 1) (getCompressV imagen) f 10)))
  (if (compressed? imagen)
      (cond
        [(bitmap? imagen) (seteo imagen pixbit-d)]
        [(pixmap? imagen) (seteo imagen pixrgb-d)]
        [else (seteo imagen pixhex-d)])
      null))

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
            (getMR (cdr histograma) (car histograma))
            (getMR (cdr histograma) mayor))))
  (getMR (cdr histograma) (car histograma)))

;Dom: pixel X lista
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

;Dom: pixel X lista
;Rec: lista
;Descripcion: la funcion agrega el color a la lista histograma en caso de no estar y si esta se agrega uno a donde corresponde
(define (agregar pixel histograma)
  (if (estaC? pixel histograma)
      (agregar1 (getColor pixel) histograma)
      (append histograma (list (list (getColor pixel) 1)))))

;Dom: [int | string | lista] X lista
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

;Dom: pixel X lista
;Rec: lista
;Descripcion: la funcion agrega el pixel a la lista depto en caso de no estar y si esta se agrega a la lista correspondiente
(define (agregarDepto pixel depto)
  (if (estaDepto? pixel depto)
      (agregarD pixel depto)
      (append depto (list (list pixel)))))

;Dom: lista
;Rec: lista
;Descripcion: agrega el pixel a la lista que corresponde
;Recursion: natural
;Justificacion: facilita la modificacion directa de alguno de los elementos de la lista ya que se trabaja uno a uno
(define (agregarD pixel depto)
  (if (null? depto)
      null
      (if (equal? (getDepth pixel) (getDepth (car (car depto))))
      (cons (append (car depto) (list pixel)) (agregarD pixel (cdr depto)))
      (cons (car depto) (agregarD pixel (cdr depto))))))

;Dom: lista
;Rec: boolean
;Descripcion: entrega un booleando que indica si esta la profundidad dentro de la lista depto
;Recursion: cola
;Justificacion: al no necesitar estados pendientes se recorre la lista hasta encontrar el color o caso contrario retornar falso
(define (estaDepto? pixel depto)
  (if (null? depto)
      #f
      (if (equal? (getDepth pixel) (getDepth (car (car depto))))
          #t
          (estaDepto? pixel (cdr depto)))))

;Dom: int
;Rec: int
;Descripcion: agrega 1
(define (incCh c) (+ c 1))

;Dom: lista de pixeles
;Rec: pixel
;Descripcion: entrega el pixel de una posicion especifica
;Recursion: cola
;Justificacion: permite recorrer y entregar un pixel de una posicion especifica
(define (entregaP pixeles x y)
  (if (null? pixeles)
      null
      (if (and (= (getPosX (firstPix pixeles)) x) (= (getPosY (firstPix pixeles)) y))
               (firstPix pixeles)
               (entregaP (nextPix pixeles) x y))))

;Dom: image X string X int X int X int X int X funcion
;Rec: string
;Descripcion: entrega la imagen en formato string
;Recursion: cola
;Justificacion: sirve para construir el string al mismo tiempo que se recorre la imagen
(define (formarString img str x0 y0 x1 y1 f)
  (if (> y0 y1)
      str
      (if (> x0 x1)
          (formarString img (string-append str "\n") 0 (+ y0 1) x1 y1 f)
          (formarString img (string-append str (f (entregaP (getPixeles img) x0 y0)) " ") (+ x0 1) y0 x1 y1 f))))

;Dom: lista de pixeles X int X int X int X int X [int | string | lista] X funcion
;Rec: lista de pixeles
;Descripcion: rellena una lista de pixeles 
;Recursion: natural
;Justificacion: se va contruyendo la lista de pixeles a medida que se llama la funcion facilitando crear un pixel que no existia antes en la lista
(define (rellenarPix pixeles x0 y0 x1 y1 color f depth)
  (if (> y0 y1)
      null
      (if (> x0 x1)
          (rellenarPix pixeles 0 (+ y0 1) x1 y1 color f depth)
          (if (null? (entregaP pixeles x0 y0))
              (cond
                [(pixrgb-d? (firstPix pixeles)) (cons (f x0 y0 (car color) (cadr color) (caddr color) depth) (rellenarPix pixeles (+ x0 1) y0 x1 y1 color f depth))]
                [else (cons (f x0 y0 color depth) (rellenarPix pixeles (+ x0 1) y0 x1 y1 color f depth))])
              (cons (entregaP pixeles x0 y0) (rellenarPix pixeles (+ x0 1) y0 x1 y1 color f depth))))))

;exportacion de funciones para su posterior uso

(provide (all-defined-out))
