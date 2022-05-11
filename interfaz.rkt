#lang racket
(require (lib "graphics.ss" "graphics"))
(open-graphics)

(define ancho 800) ;ancho de la ventana
(define alto 600) ;alto de la ventana


(define ventana (open-viewport "TicTacToe!" 800 600))



(define jugador 0) ;numero de jugador para jugador
(define maquina 1) ;numero de jugador para máquina

(define (CreaFila fila columna max)
  (if (< columna max)
      (CreaFila (append fila (list 0)) (+ 1 columna) max)
      fila
      )
  )

(define (CreaMatriz matriz fila dimensiones)
  (if (< fila (car dimensiones))
      (CreaMatriz
       (append matriz (list (CreaFila (list) 0 (cadr dimensiones))))
       (+ 1 fila)
       dimensiones)
      matriz
      )
  )



;tiñe el fondo de negro
(define (RellenarFondo color)
  ((draw-solid-rectangle ventana) (make-posn 0 0) ancho alto color)
  )


(define (DibujarHorizontales actual max)

  (when (< actual max)
      (
       (draw-line ventana)
       (make-posn 250 (+ 50 (* actual (/ 500 max))))
       (make-posn 750 (+ 50 (* actual (/ 500 max))))
       "white"
       )
    (DibujarHorizontales (+ 1 actual) max)
      )
  )

(define (DibujarVerticales actual max)

  (when (< actual max)
      (
       (draw-line ventana)
       (make-posn (+ 250 (* actual (/ 500 max))) 50)
       (make-posn (+ 250 (* actual (/ 500 max))) 550)
       "white"
       )
    (DibujarVerticales (+ 1 actual) max)
      )
  )

(define (TurnoDe turnado)
  (if (= 0 turnado)
      ((draw-string ventana) (make-posn 20 20) "Turno: jugador" "white")
      ((draw-string ventana) (make-posn 20 20) "Turno: IA" "white")
      )
  )

(define (Ganador ganador)
  (if (= 0 ganador)
      ((draw-string ventana) (make-posn 20 300) "Ganador: jugador" "white")
      ((draw-string ventana) (make-posn 20 300) "Ganador: IA" "white")
      )
  )

(define (DibujarJuego m n)
  (RellenarFondo (make-rgb 0 0 0))
  (DibujarHorizontales 1 m)
  (DibujarVerticales 1 n)
  ((draw-line ventana) (make-posn 200 0) (make-posn 200 600) "white")
  (TurnoDe jugador)
  )

(define (DibujaEquis posicion m n)
  (print posicion)
  ((draw-line ventana) (make-posn (+ 260 (/ (* 500 (car posicion)) n))
                                  (+ 60 (/ (* 500 (cadr posicion)) m))
                                  )
                       (make-posn (+ 240 (/ (* 500 (+ 1 (car posicion))) n))
                                  (+ 40 (/ (* 500 (+ 1 (cadr posicion))) m))
                                  )
                       "white"
                       )
  
  ((draw-line ventana) (make-posn (+ 260 (/ (* 500 (car posicion)) n))
                                  (+ 40 (/ (* 500 (+ 1 (cadr posicion))) m))
                                  )
                       (make-posn (+ 240 (/ (* 500 (+ 1 (car posicion))) n))
                                  (+ 60 (/ (* 500 (cadr posicion)) m))
                                  )
                       "white"
                       )
  )

(define (Bucle detener m n)
  (unless detener
    (if (not (left-mouse-click?(get-mouse-click ventana)))
        (Bucle detener m n)
        (when (and (< (posn-x (query-mouse-posn ventana)) 750)
                   (> (posn-x (query-mouse-posn ventana)) 250)
                   (> (posn-y (query-mouse-posn ventana)) 50)
                   (< (posn-y (query-mouse-posn ventana)) 550)
                   )
          (
           (DibujaEquis (list
                         (- (ceiling (/ (* n (- (posn-x (query-mouse-posn ventana)) 250)) 500)) 1)
                         (- (ceiling (/ (* m (- (posn-y (query-mouse-posn ventana)) 50)) 500)) 1))
                        m n
                        )
           (Bucle detener m n)
           )
           ;(print (list (posn-y (query-mouse-posn ventana))))
           ;(print (list (- (ceiling (/ (* n (- (posn-x (query-mouse-posn ventana)) 250)) 500)) 1) (- (ceiling (/ (* m (- (posn-y (query-mouse-posn ventana)) 50)) 500)) 1)))

          )
        )
    )
  )

(define (Juego m n)
  (define matriz (CreaMatriz (list ) 0 (list m n))) ;crea una matriz de ceros
  (DibujarJuego m n)
  (Bucle #f m n)
  )
;(Juego 3 3)