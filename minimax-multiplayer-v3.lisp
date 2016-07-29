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

;ESTE CODIGO CORRESPONDE A LA VERSION 3 DE MINIMAX-MULTIPLAYER, EN LA QUE SE INCLUYE LA POSIBILIDAD DE ESTABLECER UNA PROFUNDIDAD LIMITE, UN TIEMPO LIMITE (SEGUNDOS) O AMBOS LIMITES PARA LA EXPLORACION DEL ARBOL DEL JUEGO.

(defun decision-maxn (actual profundidad tiempo) ;Para omitir la profundidad o el tiempo basta con pasar un valor negativo.
  "devuelve el nodo sucesor correspondiente al movimiento mejor valorado para el jugador que lo invoca, para ello se construye de manera reursiva el arbol del juego"
  (let ((max-val *minimo-valor*) (max-nodo ()) (jugador (turno actual)) (instante-inicial (get-universal-time)))
    (loop for nodo in (sucesores actual)
       do (let* ((puntuacion (valor-maxn nodo max-val (1- profundidad) tiempo instante-inicial)))
            (if (>= (puntuacion-jugador puntuacion jugador) max-val)
                (progn (setf max-val (puntuacion-jugador puntuacion jugador))
                       (setf max-nodo nodo))))
       if (or (= max-val *maximo-valor*) (tiempo-expirado tiempo instante-inicial)) do (loop-finish))
    max-nodo)
)

(defun valor-maxn (nodo cota-puntos profundidad tiempo instante-inicial)
  "devuelve la puntuacion del nodo sucesor mejor valorado para el jugador del nodo actual"
  (if (or (es-estado-final (estado nodo)) (not (sucesores nodo)) (eq profundidad 0))
      (evaluacion-estatica (estado nodo) (turno nodo))
      (maximiza-puntuacion (sucesores nodo) (turno nodo) cota-puntos (1- profundidad) tiempo instante-inicial))
)

(defun maximiza-puntuacion (sucesores jugador cota-puntos profundidad tiempo instante-inicial)
  "devuelve la puntuacion maxima de entre las puntuaciones de los nodos sucesores, para el jugador pasado como parametro"
  (let ((max-val *minimo-valor*) (max-puntuacion ()))
        (loop for nodo in sucesores
           do (let* ((puntuacion (valor-maxn nodo max-val profundidad tiempo instante-inicial)))
                (if (>= (puntuacion-jugador puntuacion jugador) max-val)
                    (progn (setf max-val (puntuacion-jugador puntuacion jugador))
                           (setf max-puntuacion puntuacion))))
           if (or (<= (- *maximo-valor* max-val) cota-puntos) (tiempo-expirado tiempo instante-inicial)) do (loop-finish))
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

(defun siguiente-turno (turno-actual)
  "devuelve el identificador del jugador al que le toca mover en el siguiente turno"
  (if (eq turno-actual *numero-jugadores*) 1 (1+ turno-actual))
)

(defun tiempo-transcurrido (instante-inicial)
  "devuelve el tiempo transcurrido entre el instante inicial pasado como parametro y el instante actual, en segundos"
  (- (get-universal-time) instante-inicial)
)

(defun tiempo-expirado (tiempo instante-inicial)
  "indica si el tiempo pasado como parametro ha expirado"
  (and (>= tiempo 0) (<= tiempo (tiempo-transcurrido instante-inicial)))
  ;Comprobar que el tiempo sea mayor o igual que cero posibilita que, al pasar un valor negativo de tiempo, este nunca expire.
  ; El objetivo de esto es desactivar el tiempo limite pasando un valor negativo.
)
