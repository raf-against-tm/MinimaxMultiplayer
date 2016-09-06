;Algorimo de decision minimax extendido para aplicarlo a multiples jugadores. Renombrado como decision-maxn.


;Para hacer uso del algoritmo se supone que ha sido cargada, previamente, la implementacion de la especificacin del juego sobre
; el que se va aplicar el algoritmo. En toda especificacion de cualquier juego se representara cada jugador por un numero, de manera que,
; sea mas sencillo identificarlo y obtener su puntuacion correspondiente en el vector de puntuciones devuelto por la funcion de evaluacion estatica.
;
; Dicha implementación debe incluir las siguientes variables globales:
;   - *minimo-valor*
;   - *maximo-valor*
;   + movimientos (estado turno)
;   + es-estado-final(estado)
;   + evaluacion-estatica(estado turno)
;   + construye-nodo(estado turno)
;   + estado(nodo)
;   + turno(nodo)

(defun decision-maxn (actual)
	"devuelve el nodo sucesor correspondiente al movimiento mejor valorado para el jugador que lo invoca"
	(let ((max-val *minimo-valor*) 
		  (jugador (turno actual))
		  (max-nodo nil))
		  
		(loop for nodo in (sucesores actual)
		  do (let ((puntuacion (valor-maxn nodo)))
		  
				 (if (>= (puntuacion-jugador puntuacion jugador) max-val)
					 (progn (setf max-val (puntuacion-jugador puntuacion jugador))
							(setf max-nodo nodo)))))
							
    max-nodo)
	
)

(defun valor-maxn (nodo)
	"devuelve la puntuacion del nodo sucesor mejor valorado para el jugador del nodo actual"
	(if (or (es-estado-final (estado nodo)) (not (sucesores nodo)))
		(evaluacion-estatica (estado nodo) (turno nodo))
		
		(maximiza-puntuacion (sucesores nodo) (turno nodo)))
		
)

(defun maximiza-puntuacion (sucesores jugador)
	"devuelve la puntuacion maxima de entre las puntuaciones de los nodos sucesores, para el jugador pasado como parametro"
	(let ((max-val *minimo-valor*) 
		  (max-puntuacion nil))
		  
        (loop for nodo in sucesores
          do (let ((puntuacion (valor-maxn nodo)))
		  
				 (if (>= (puntuacion-jugador puntuacion jugador) max-val)
					 (progn (setf max-val (puntuacion-jugador puntuacion jugador))
							(setf max-puntuacion puntuacion)))))
							
		max-puntuacion)
   
)

(defun sucesores (nodo)
  "obtiene la lista de sucesores del nodo dado, a partir de los posibles movimientos del jugador al que le toca mover"
  (loop for movimiento in (movimientos (estado nodo) (turno nodo))
       if (not (equal (sucesor nodo movimiento) 'no-aplicable))
           collect (sucesor nodo movimiento) into lista-sucesores
		   
	   ;Si no puede aplicar ningun movimiento y el estado no es final debe pasar el turno, por tanto el sucesor es el mismo estado actual.
	   finally (if (and (eq lista-sucesores nil) (not (es-estado-final (estado nodo))))
				   (return (list (construye-nodo (estado nodo) (siguiente-turno (turno nodo)))))
				   (return lista-sucesores)))
				   
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
	"a partir del vector de puntuacion, que contiene un valor por cada puntuacion de cada jugador, 
		devuelve la puntuacion correspondiente al jugador especificado"
	(nth (1- jugador) puntuacion)
	
)

(defun siguiente-turno (turno-actual)
	"devuelve el identificador del jugador al que le toca mover en el siguiente turno"
	(if (eq turno-actual *numero-jugadores*) 1 (1+ turno-actual))
	
)
    