#lang racket
(require (lib "graphics.ss" "graphics"))
;(include Greedy.rkt)
(open-graphics)

(define ancho 800) ;ancho de la ventana
(define alto 600) ;alto de la ventana


(define ventana null)



(define jugador 0) ;numero de jugador para jugador
(define maquina 1) ;numero de jugador para máquina
(define turno 0)   ;jugador que está en turno. por defecto empieza el humano

(define mat null)
(define m 0)
(define n 0)

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
  (set! turno turnado)
  (DibujarJuego)
  (if (= jugador turnado)
      ((draw-string ventana) (make-posn 20 20) "Turno: jugador" "white")
      ((draw-string ventana) (make-posn 20 20) "Turno: IA" "white")
      )
  )

(define (Ganador ganador)
  (if (= jugador ganador)
      ((draw-string ventana) (make-posn 20 300) "Ganador: jugador" "white")
      ((draw-string ventana) (make-posn 20 300) "Ganador: IA" "white")
      )
  )

(define (DibujaProgreso i j)
  (when (< i m)
    (cond 
        [(< j n)
         (when (= (buscar? mat 0 0 i j) (+ jugador 1))
           
           (DibujaCirculo (list j i))
           )
         (DibujaProgreso i (+ j 1))
         ]
        [else
         (DibujaProgreso (+ i 1) 0)]
        )
    )
  )

(define (DibujarJuego)
  (RellenarFondo (make-rgb 0 0 0))
  (DibujarHorizontales 1 m)
  (DibujarVerticales 1 n)
  ((draw-line ventana) (make-posn 200 0) (make-posn 200 600) "white")
  (DibujaProgreso 0 0)
  )

(define (DibujaCirculo posicion)
  ((draw-ellipse ventana) (make-posn (+ 260 (/ (* 500 (car posicion)) n))
                                  (+ 60 (/ (* 500 (cadr posicion)) m))
                                  )
                          (- (/ 500 n) 20)
                          (- (/ 500 m) 20)
                          "white"
                          )
  )

(define (DibujaEquis posicion)
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

 (define (buscar? matriz i j m n)
  (cond ((equal? matriz '()) -1)
        ((and (equal? i m) (equal? j n)) (buscarAux matriz i j))
        ((equal? #f (equal? i m)) (buscar? (cdr matriz) (+ i 1) j m n))
        ((equal? #f (equal? j n)) (buscarFila? (car matriz) j n i m))))

(define (buscarFila? fila j n i m)
  (cond ((equal? fila '()) (buscar? '() 0 0 0 0) )
        ((equal? n 0) (buscar? (cdr fila) i j m n))
        ((equal? j n) (buscar? fila i j m n))
        (else (buscarFila? (cdr fila) (+ j 1) n i m))))

(define (buscarAux fila i j) 
  (cond ((equal? (list? fila) (list? (car fila))) (caar fila))
        ((list? fila) (car fila))))

(define (cambiarFila fila lista i j m n dato)
  (cond ((null? fila) lista)
        ((and (equal? i m) (equal? j n)) (cambiarFila (cdr fila) (append lista (list dato)) i (+ j 1) m n dato))
        (else (cambiarFila (cdr fila) (append lista (list (car fila)))  i (+ j 1) m n dato))))

(define (cambiarDatoMatriz matriz i j m n nuevaMatriz dato)
  (cond ((equal? matriz '()) nuevaMatriz)
        (else (cambiarDatoMatriz (cdr matriz) (+ i 1) 0 m n (append nuevaMatriz (list (cambiarFila (car matriz) '() i 0 m n dato))) dato))))


(define (Bucle detener)
  (unless detener
    (cond
      [(= turno jugador)
        (cond
          [(not (left-mouse-click?(get-mouse-click ventana)))
            (Bucle detener)
            ]
            [else
             (when (and
                   (< (posn-x (query-mouse-posn ventana)) 750)
                   (> (posn-x (query-mouse-posn ventana)) 250)
                   (> (posn-y (query-mouse-posn ventana)) 50)
                   (< (posn-y (query-mouse-posn ventana)) 550)
                   )
               (cond
                 [(= 0 (buscar? mat 0 0 (- (ceiling (/ (* m (- (posn-y (query-mouse-posn ventana)) 50)) 500)) 1) (- (ceiling (/ (* n (- (posn-x (query-mouse-posn ventana)) 250)) 500)) 1)))
                  (set! mat (cambiarDatoMatriz mat
                                           0 0
                                           (- (ceiling (/ (* m (- (posn-y (query-mouse-posn ventana)) 50)) 500)) 1)
                                           (- (ceiling (/ (* n (- (posn-x (query-mouse-posn ventana)) 250)) 500)) 1)
                                           (list) 1)
                    )
                  (TurnoDe maquina)
                  ]
                 [else
                  ((draw-string ventana) (make-posn 20 200) "Elija otro espacio" "pink")
                  ]
                 )
              )
            ]
          )
        ]
        ;(Main mat m n)
        [else
         (TurnoDe jugador)
         ]
        )
    (Bucle detener)
    )
  )
        

(define (Juego eme ene)
  (cond
    [(or (< eme 3) (< ene 3))
     (print "La matriz debe ser mínimo (3x3)")
     ]
    [else
     (cond
       [(or (> eme 10) (> ene 10))
        (print "La matriz excede el límite (10x10)")
        ]
       [else
        
        (set! m eme)
        (set! n ene)
        (set! mat (CreaMatriz (list ) 0 (list m n))) ;crea una matriz de ceros
        (set! ventana (open-viewport "TicTacToe!" 800 600))
        (TurnoDe jugador)
        (Bucle #f)
        ]
       )
     ]
    )
  )