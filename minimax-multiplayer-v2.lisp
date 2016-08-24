; Algorimo de decision minimax extendido para aplicarlo a multiples jugadores. Renombrado como decision-maxn.

;Para hacer uso del algoritmo se supone que ha sido cargada, previamente, la implementacion de la especificacin del juego sobre
; el que se va aplicar el algoritmo. En toda especificacion de cualquier juego se representara cada jugador por un numero, de manera que,
; sea mas sencillo identificarlo y obtener su puntuacion correspondiente en el vector de puntuciones devuelto por la funcion de evaluacion estatica.

;  Dicha implementación debe incluir las siguientes variables globales:
;   - *minimo-valor*
;   - *maximo-valor*
;	- *maxima-suma*
;   + movimientos (estado turno)
;   + es-estado-final(estado)
;   + evaluacion-estatica(estado turno)
;   + construye-nodo(estado turno)
;   + estado(nodo)
;   + turno(nodo)
;   + siguiente-turno(turno)

;ESTE CODIGO CORRESPONDE A LA VERSION 2 DE MINIMAX-MULTIPLAYER, A LA QUE SE LE APLICA PODA INMEDIATA Y PODA SUPERFICIAL

;La poda inmediata consiste en que si un movimiento supone la maxima puntuacion *maximo-valor* para un jugador, entonces,
; no es neceserio evaluar el resto de nodos, pues es imposible que vaya a mejorar dicha puntuacion.

;La poda superficial sigue la regla (*maxima-suma* - y <= x) siendo 'x' el maximo valor hasta el momento del nodo padre e y el maximo valor 
; hasta el momento del nodo hijo correspondiente a ese padre. *maxima-suma* es el total maximo de la suma de las puntuaciones de todos los jugadores
; en un estado concreto del juego.
;
;El objetivo de la poda superficial es la comparacion de puntuaciones maximas entre el nodo padre (jugador que mueve antes) y 
; el nodo  hijo (jugador que mueve a continuacion) de manera que, si se cumple la regla anterior, el jugador correspondiente al nodo padre no va 
; a escoger el movimiento hacia ese nodo hijo luego no tiene sentido seguir evaluando nodos sucesores de ese hijo.
;
;Ademas, la poda superficial depende del tipo de puntuaciones que tenga el juego. Segun lo expuesto en el recurso R4, si en el juego no se cumple 
; la desigualdad (maxima-suma < 2 * maximo-valor) no se lleva a cabo la poda superficial y solo podría hacer por tanto, poda inmediata.
;
;R4. Sturtevant, N., Korf, R.: On Pruning Techniques for Multi-Player Games. In: AAAI 2000, pp. 201-207 (2000).

(defun decision-maxn (actual)
	"devuelve el nodo sucesor correspondiente al movimiento mejor valorado para el jugador que lo invoca"
	(let ((max-val *minimo-valor*)  
		  (jugador (turno actual))
		  (max-nodo nil))
		  
		(loop for nodo in (sucesores actual)
          do (let ((puntuacion (valor-maxn nodo max-val)))
		  
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
	(let ((max-val *minimo-valor*) 
		  (max-puntuacion nil))
		  
        (loop for nodo in sucesores
          do (let ((puntuacion (valor-maxn nodo max-val)))
		  
				 (if (>= (puntuacion-jugador puntuacion jugador) max-val)
					 (progn (setf max-val (puntuacion-jugador puntuacion jugador))
							(setf max-puntuacion puntuacion))))

		  if (<= (- *maxima-suma* max-val) cota-puntos) do (loop-finish)) ;Poda superficial (incluye poda inmediata en su caso)
		    
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
