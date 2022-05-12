#lang racket
(define mat (list (list 1 0 0) (list 0 0 0) (list 0 2 0)))

;;Verifica si la matriz no esta completa, para poder continuar el juego
;;Parametros
;;matriz de tamaño mxn
(define (verificarMatriz matriz)
  (cond ((null? matriz) #t)
        (else (verificarFila (car matriz) matriz))))

(define (verificarFila fila matriz)
  (cond ((null? fila) (verificarMatriz (cdr matriz)))
        ((equal? (car fila) 0) #f)
        (else (verificarFila (cdr fila) matriz))))

;; Parametros matriz, es la matriz n x
;;i, j son 0, contadores para buscar en la matriz
;;n y m son las posiciones que se quieren encontrar, m la fila y n la columna
;;retorna el valor de la posicion (m,n)
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


;;Dada una posición verifica si 2 posiciones a la derecha son iguales al parametro jugador
;;Parametros
;;posj, posi son las posiciones del caracter a evular, son enteros
;;jugadr es entero y representa si es el jugador o la maquina

(define (ganarDerecha posj posi jugador matriz) 
  (cond ((and (equal? (buscar? matriz 0 0 posj (+ posi 1)) (buscar? matriz 0 0 posj (+ posi 2))) (equal? (buscar? matriz 0 0 posj (+ posi 1)) jugador)) #t)
        (else #f)))


;;Dada una posición verifica si 2 posiciones hacia abajo son iguales al parametro jugador
;;Parametros
;;posj, posi son las posiciones del caracter a evular, son enteros
;;jugadr es entero y representa si es el jugador o la maquina

(define (ganarAbajo posj posi jugador matriz) 
  (cond ((and (equal? (buscar? matriz 0 0 (+ posj 1) posi) (buscar? matriz 0 0 (+ posj 2) posi)) (equal? (buscar? matriz 0 0 (+ posj 1) posi) jugador)) #t)
        (else #f)))

;;Dada una posición verifica si 2 posiciones en diagonal hacia abajo-derecha son iguales al parametro jugador
;;Parametros
;;posj, posi son las posiciones del caracter a evular, son enteros
;;jugadr es entero y representa si es el jugador o la maquina


(define (ganarAbajoDerecha posj posi jugador matriz) 
  (cond ((and (equal? (buscar? matriz 0 0 (+ posj 1) (+ posi 1)) (buscar? matriz 0 0 (+ posj 2) (+ posi 2))) (equal? (buscar? matriz 0 0 (+ posj 1) (+ posi 1)) jugador)) #t)
        (else #f)))

;;Dada una posición verifica si 2 posiciones en diagonal hacia abajo-izquierda son iguales al parametro jugador
;;Parametros
;;posj, posi son las posiciones del caracter a evaluar, son enteros
;;jugadr es entero y representa si es el jugador o la maquina

(define (ganarAbajoIzquierda posj posi jugador matriz) 
  (cond ((and (equal? (buscar? matriz 0 0 (+ posj 1) (- posi 1)) (buscar? matriz 0 0 (+ posj 2) (- posi 2))) (equal? (buscar? matriz 0 0 (+ posj 1) (- posi 1)) jugador)) #t)
        (else #f)))

;Arreglar el return porque se salta los demas elementos de la fila actualmente
(define (Element? original_mat row1 j i)
  (cond [(null? row1)#f]
        [(equal? (car row1) 1) (Element?Aux original_mat 1 i j row1)]
        [(equal? (car row1) 2) (Element?Aux original_mat 2 i j row1)]
        (else (Element? original_mat (cdr row1) (+ j 1) i))))

(define (Element?Aux original_mat player i j row1)
  
  (cond [(equal? (ganarAbajoIzquierda j i player original_mat) #t) (cond [(equal? player 1) "PlayerWins"]
                                                                         (else "CPUWins"))]
        [(equal? (ganarAbajoDerecha j i player original_mat) #t) (cond [(equal? player 1) "PlayerWins"]
                                                                         (else "CPUWins"))]
        [(equal? (ganarDerecha j i player original_mat) #t) (cond [(equal? player 1) "PlayerWins"]
                                                                         (else "CPUWins"))]
        [(equal? (ganarAbajo j i player original_mat) #t) (cond [(equal? player 1) "PlayerWins"]
                                                                         (else "CPUWins"))]
        (else (Element? original_mat (cdr row1) (+ j 1) i))))

(define (Winner? original_mat matrix m n i)
  
  (cond [(null? matrix)#f]
        [(equal? i m)(cond [(equal? (Element? original_mat matrix 0 i) "PlayerWins") "PlayerWins"]
                           [(equal? (Element? original_mat matrix 0 i) "CPUWins") "CPUWins"]
                           (else #f))]
        (else(cond [(equal? (Element? original_mat (car matrix) 0 i) "PlayerWins") "PlayerWins"]
                   [(equal? (Element? original_mat (car matrix) 0 i) "CPUWins") "CPUWins"]
                   (else (Winner? original_mat (cdr matrix) m n (+ i 1)))))))

;;Funcion main que verifica victoria o empate

(define (Main original_mat m n)
  (cond [(equal? (Winner? original_mat original_mat m n 0) #t)(Winner? original_mat original_mat m n 0)]
        [(equal? (verificarMatriz original_mat) #t)"Tie"]
        (else (GreedySel original_mat m n 0 0))))


;Funciones de seleccion

(define (GreedySel original_mat m n i j)
  (print (verificarMovimientoComputadora i j original_mat))
  (cond [(and (equal? (buscar? original_mat 0 0 i j) 1) (equal? (car (verificarMovimientoComputadora i j original_mat)) #f)) (cond [(and (equal? m (+ i 1)) (equal? n (+ j 1))) (RandomPos m n original_mat)]
                                                                                   (else
                                                                                   (cond [(equal? n (+ j 1)) (GreedySel original_mat m n (+ 1 i) 0)]
                                                                                   (else (GreedySel original_mat m n i (+ 1 j))))))]
        [(and (equal? (buscar? original_mat 0 0 i j) 1) (equal? (car (verificarMovimientoComputadora i j original_mat)) #t)) (list (caddr (verificarMovimientoComputadora i j original_mat)) (cadr (verificarMovimientoComputadora i j original_mat)))]

        ;[(and (equal? (buscar? original_mat 0 0 i j) 2) (equal? (car (verificarMovimientoComputadora2 i j original_mat)) #f)) (agregar2 i j m n original_mat)]
        ;[(and (equal? (buscar? original_mat 0 0 i j) 2) (equal? (car (verificarMovimientoComputadora2 i j original_mat)) #t)) (list (caddr (verificarMovimientoComputadora2 i j original_mat)) (cadr (verificarMovimientoComputadora i j original_mat)))]
        [(equal? (buscar? original_mat 0 0 i j) 2)(agregar2 i j m n original_mat)]
        [(and (equal? m (+ i 1)) (equal? n (+ j 1))) (RandomPos m n original_mat)]
        (else (cond [(and (equal? m (+ i 1)) (equal? n (+ j 1))) (RandomPos m n original_mat)]
              (else
              (cond [(equal? n (+ j 1)) (GreedySel original_mat m n (+ 1 i) 0)]
              (else (GreedySel original_mat m n i (+ 1 j)))))))
))

(define (RandomPos m n matriz)
  (RandomPosAux (random 0 m) (random 0 n) m n matriz)
  )

(define (RandomPosAux i j m n matriz)
  (cond (equal? (buscar? matriz 0 0 i j) 0) (list i j)
  (else (RandomPosAux (random m) (random n) m n matriz)))
  )

;; Funciones de verificacion para bloquear al jugador de ganar

(define (verificarDerecha posi posj matriz)
  (cond ((equal? (buscar? matriz 0 0 posj (+ posi 1)) 1) 2)
        ((equal? (buscar? matriz 0 0 posj (+ posi 2)) 1) 1)
        (else 0)))

(define (verificarAbajo posi posj matriz)
  (cond ((equal? (buscar? matriz 0 0 (+ posj 1) posi) 1) 2)
        ((equal? (buscar? matriz 0 0 (+ posj 2) posi) 1) 1)
        (else 0)))

(define (verificarAbajoDerecha posi posj matriz)
  (cond ((equal? (buscar? matriz 0 0 (+ posj 1) (+ posi 1)) 1) 2)
        ((equal? (buscar? matriz 0 0 (+ posj 2) (+ posj 2)) 1) 1)
        (else 0)))

(define (verificarAbajoIzquierda posi posj matriz)
  (cond ((equal? (buscar? matriz 0 0 (+ posj 1) (- posi 1)) 1) 2)
        ((equal? (buscar? matriz 0 0 (+ posj 2) (- posj 2)) 1) 1)
        (else 0)))

(define (verificarMovimientoComputadora posi posj matriz)
  (cond ((equal? #f (equal? 0 (verificarDerecha posi posj matriz))) (list #t (+ posi (verificarDerecha posi posj matriz))0))
        ((equal? #f (equal? 0 (verificarAbajo posi posj matriz))) (list #t 0 (+ posj (verificarAbajo posi posj matriz))))
        ((equal? #f (equal? 0 (verificarAbajoDerecha posi posj matriz))) (list #t (+ posi (verificarAbajoDerecha posi posj matriz)) (+ posj (verificarAbajoDerecha posi posj matriz))))
        ((equal? #f (equal? 0 (verificarAbajoIzquierda posi posj matriz))) (list #t (+ posi (verificarAbajoIzquierda posi posj matriz)) (- posj (verificarAbajoIzquierda posi posj matriz))))
        (else (list #f posi posj))))


;;;Funcion para jugabilidad del CPU alrededor de sus anteriores

(define (agregar2 i j m n matriz)
  (cond ((equal? (buscar? matriz 0 0 i (- j 1)) 0) (list i (- j 1)))
        ((equal? (buscar? matriz 0 0 i (+ j 1)) 0) (list i (+ j 1)))
        ((equal? (buscar? matriz 0 0 (+ i 1) (+ j 1)) 0) (list (+ i 1) (+ j 1)))
        ((equal? (buscar? matriz 0 0 (- i 1) (- j 1)) 0) (list (- i 1) (- j 1)))
        (else RandomPos m n matriz)
        ))

  


(Main mat 3 3)





  