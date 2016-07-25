; Algorimo de decision minimax extendido para aplicarlo a multiples jugadores. Renombrado como decision-maxn.

;Para hacer uso del algoritmo se supone que ha sido cargada, previamente, la implementacion de la especificacin del juego sobre el que se va aplicar el algoritmo.

;En toda especificacion de cualquier juego se representara cada jugador por un numero, de manera que sea mas sencillo identificarlo y obtener su puntuacion correspondiente en el vector de puntuciones devuelto por la funcion de evaluacion estatica.

;  Dicha implementación debe incluir las siguientes variables globales:
;   - *minimo-valor*
;   - *maximo-valor*
;   - *movimientos*
;   + es-estado-final(estado)
;   + evaluacion-estatica(estado turno)
;   + construye-nodo(estado turno)
;   + estado(nodo)
;   + turno(nodo)
;   + siguiente-turno(turno)

;ESTE CODIGO CORRESPONDE A LA VERSION 2 DE MINIMAX-MULTIPLAYER, A LA QUE SE LE APLICA PODA INMEDIATA Y PODA SUPERFICIAL

;La poda inmediata consiste en que si un movimiento supone la maxima puntuacion *maxp* para un jugador, entonces no es neceserio evaluar el resto de nodos, pues es imposible que vaya a mejorarse dicha puntuacion.

;La poda superficial sigue la regla (*maximo-valor* - y <= x) siendo 'x' el maximo valor hasta el momento de un nodo padre e y el maximo valor hasta el momento del nodo hijo correspondiente a ese padre.
;El objetivo de la poda superficial es la comparacion de puntuaciones maximas entre el nodo padre (jugador que mueve antes) y el nodo  hijo (jugador que mueve a continuacion) de manera que, si se cumple la regla anterior, el jugador correspondiente al nodo padre no va a escoger el movimiento hacia ese nodo hijo luego no tiene sentido seguir evaluando nodos sucesores de ese hijo.

(defun decision-maxn (actual)
  "devuelve el nodo sucesor correspondiente al movimiento mejor valorado para el jugador que lo invoca, para ello se construye de manera reursiva el arbol del juego"
  (let ((max-val *minimo-valor*) (max-nodo ()) (jugador (turno actual)))
    (loop for nodo in (sucesores actual)
       do (let* ((puntuacion (valor-maxn nodo max-val)))
            (if (>= (puntuacion-jugador puntuacion jugador) max-val)
                (progn (setf max-val (puntuacion-jugador puntuacion jugador))
                       (setf max-nodo nodo))))
       if (= max-val *maximo-valor*) do (loop-finish)) ;Poda inmediata.
       ;En este punto solo tiene sentido la poda inmediata pues el nodo actual no tiene antecesor.
    max-nodo)
)

(defun valor-maxn (nodo cota-puntos) ;cota-puntos indica la puntuacion maxima, en ese momento,
                                     ;  para el padre del nodo pasado como parametro, 
                                     ;  posibilitando la poda superficial en su caso.
  "devuelve la puntuacion del nodo sucesor mejor valorado para el jugador del nodo actual"
  (if (or (es-estado-final (estado nodo)) (not (sucesores nodo)))
      (evaluacion-estatica (estado nodo) (turno nodo))
      (maximiza-puntuacion (sucesores nodo) (turno nodo) cota-puntos))
)

(defun maximiza-puntuacion (sucesores jugador cota-puntos)
  "devuelve la puntuacion maxima de entre las puntuaciones de los nodos sucesores, para el jugador pasado como parametro"
  (let ((max-val *minimo-valor*) (max-puntuacion ()))
        (loop for nodo in sucesores
           do (let* ((puntuacion (valor-maxn nodo max-val)))
            (if (>= (puntuacion-jugador puntuacion jugador) max-val)
                (progn (setf max-val (puntuacion-jugador puntuacion jugador))
                       (setf max-puntuacion puntuacion))))
           ;En este punto solo tiene sentido la poda superficial, pues si se da el caso de puntuacion maxima para el jugador que mueve, 
           ; entonces la condicion se cumple para cualquier cota que establezca el jugador anterior y, en ese caso, 
           ; la poda superficial poda las mismas ramas que si se hiciera poda inmediata por parte del jugador que mueve.
           if (<= (- *maximo-valor* max-val) cota-puntos) do (loop-finish)) ;Poda superficial.
   max-puntuacion)
)

(defun sucesores (nodo)
  "obtiene la lista de sucesores del nodo dado, a partir de los posibles movimientos que el juego permite al jugador que mueve en ese nodo"
  (loop for movimiento in *movimientos*
       if (not (equal (sucesor nodo movimiento) 'no-aplicable))
           collect (sucesor nodo movimiento))
)

(defun sucesor (nodo movimiento)
  "obtiene el sucesor de un estado para un nodo y un movimiento del juego dado"
  (let ((estado-sucesor (aplica-movimiento movimiento (estado nodo))))
    (if (equal estado-sucesor 'no-aplicable) 
        'no-aplicable 
        (construye-nodo estado-sucesor (siguiente-turno (turno nodo)))))
)

;Funciones auxiliares

(defun puntuacion-jugador (puntuacion jugador)
  "a partir del vector de puntuacion, que contiene un valor por cada puntuacion de cada jugador, devuelve la puntuacion correspondiente al jugador especificado"
  (nth (1- jugador) puntuacion)
)
    

