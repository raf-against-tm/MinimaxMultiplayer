;Entorno para jugar a Blokus, incluyendo informacion grafica para representar cada movimiento y las acciones de cada jugador.

;Declaracion de importaciones. Los archivos deben cargarse en el orden especificado.

(load "C:/IA1/MinimaxMultiplayer/game-blokus.lisp")
(load "C:/IA1/MinimaxMultiplayer/minimax-multiplayer-v4.lisp")

;Declaracion de constantes.
(defconstant HUMANO 	1)
(defconstant EGOISTA 	2)
(defconstant PARANOICO 	3)
(defconstant ALEATORIO 	4)

;Declaracion de variables globales

(defvar *tipos-jugadores* nil) ;Contendra una lista con los tipos de jugadores elegidos.

;Funcion principal.

(defun inicia-blokus ()
	"lanza un entorno para poder jugar a Blokus"
	
	(let ((estado-actual *estado-inicial*)
		  (estado-temporal nil)
		  (nombre-pieza-seleccionada nil)
		  (pieza-seleccionada nil)
		  (pieza-transformada nil)
		  (rotacion nil)
		  (reflejar nil)
		  (continuar nil)
		  (fila nil)
		  (columna nil)
		  (inicial nil)
		  (turno-actual 1))
		  
		 (format t 
			"~&~% BIENVENIDO A BLOKUS!
			   ~& REGLAS PARA JUGAR A BLOKUS: 
			   ~&  - Cada jugador dispone de 21 piezas que deberá colocar en un tablero de 20x20. 
			   ~&  - Las piezas de un mismo jugador solo pueden colocarse en aquellas posiciones en las que toque otras piezas suyas diagonalmente. 
			   ~&  - Cada jugador comienza en una esquina predefinida del tablero (en el 1er movimiento no hace falta que se cumpla la regla anterior). 
			   ~&  - La posicion inicial del jugador 1 es la esquina superior izquierda el resto de esquinas en el sentido de las agujas del reloj.
			   ~&  - El juego termina cuando ningun jugador puede colocar ninguna pieza más. 
			   ~&  - Gana el que más puntos haya conseguido, teniendo en cuenta que cada bloque ocupado por el suma 1 punto. 
			   ~&  - Si ha colocado todas sus piezas suma 15 adicionales. 
			   ~&  - Si además ha colocado en último lugar la pieza de un bloque (I1) suma 5 puntos más. ~&~%")
			  
		 (format t "~&  NOTAS: LA POSICIÓN INDICADA EN EL MOVIMIENTO SE CORRESPONDE CON LA ESQUINA SUPERIOR IZQUIERDA DE LA PIEZA. DEBE SER TENIDO EN~%")
		 (format t "			CUENTA, PUES LA POSICIÓN INICIAL DEL JUGADOR 2, POR EJEMPLO, DEPENDE DE LA PIEZA QUE COLOQUE.~2%")
  
		 (format t "~&		 LAS PIEZAS SE ENCUENTRAN EN MATRICES DE FxC INDICANDO BLOQUES RELLENOS Y VACIOS DE LA MISMA. ESTO QUIERE DECIR QUE~%")
		 (format t "          LA ESQUINA SUPERIOR IZQUIERDA DE UNA PIEZA PUEDE NO TENER UN BLOQUE RELLENO, PERO LA POSICIÓN DEL TABLERO INDICADA,~%")
		 (format t "	      SE CORRESPONDE IGUALMENTE CON LA ESQUINA SUPERIOR IZQUIERDA DE MISMA Y EN LA QUE HAY UN BLOQUE VACÍO.~2%")
					
		 (format t "~&		 EVIDENTEMENTE SI UN BLOQUE, VACIO EN LA PIEZA, SE COLOCA EN UNA POSICIÓN DEL TABLERO QUE ESTA OCUPADA, EL MOVIMIENTO~%")
	     (format t "		  PUEDE SER VÁLIDO PUES ÚNICAMENTE DEPENDE DE DÓNDE SE SITUEN LOS BLOQUES RELLENOS DE LA MISMA.")
				   
		 (inicializa-variables-globales)
		
		 (format t "~&~% TODO LISTO! EMPIEZA EL JUGADOR 1 (AA). INTRODUZCA CUALQUIER VALOR PARA COMENZAR...  ")
		 (read)
	
		 (loop while (not (es-estado-final estado-actual)) ;Bucle general del juego
		 
			do (loop for jugador from 1 to *numero-jugadores* ;Bucle correspondiente a los turnos de cada jugador.
			
				if (eq (numero-movimientos-validos estado-actual jugador) 0)
					do (format t "~& El jugador ~a (~a) no tiene movimientos posibles. Introduzca cualquier valor para continuar... "
																												jugador (color-jugador jugador))
							  
					   (read)
				
				else
					do (case (nth (1- jugador) *tipos-jugadores*)
							 (1 ;HUMANO
									(tagbody repetir-seleccion-completa
									  
									    ;SELECCION DE PIEZA
									    (tagbody repetir-seleccion-pieza
									  
										  (format t "~& A continuacion se muestran sus piezas disponibles... ~&~%")
										  (pinta-piezas-jugador (gethash jugador (estado-jugadores estado-actual)) jugador)
										  (format t "~& Indique el nombre de la pieza (I1, I2, etc...) que desea seleccionar: ")
										  (setf nombre-pieza-seleccionada (read))
										  (setf pieza-seleccionada (selecciona-pieza nombre-pieza-seleccionada))
										  
										  (loop while t ;Valida pieza seleccionada.
											if (not (find pieza-seleccionada (gethash jugador (estado-jugadores estado-actual)) :test #'equalp))
											   do (format t "~& La pieza elegida ha sido usada o no existe, por favor indique otra: ")
												  (setf nombre-pieza-seleccionada (read))
												  (setf pieza-seleccionada (selecciona-pieza nombre-pieza-seleccionada))
												  
											else
											   do (loop-finish))
											   
										  (format t "~& La pieza seleccionada es ~a: ~&~%" nombre-pieza-seleccionada)
										  (pinta-pieza pieza-seleccionada jugador)
										  
										  ;Pregunta para continuar o repetir seleccion.
										  (format t "~& Si está seguro de su seleccion indique (si) para continuar o (no) para seleccionar otra: ")
										  (setf continuar (read))
										  
										  (loop while t
											if (and (not (eq continuar 'si)) (not (eq continuar 'no)))
												do (format t "~& La respuesta introducida no es correcta, vuelva a introducir (si) o (no): ")
												   (setf continuar (read))
												   
											else
												if (eq continuar 'si) do (setf continuar nil) (loop-finish)
											
												else do (setf continuar nil) (go repetir-seleccion-pieza)))
									  
									    ;ROTACION DE PIEZA
									    (tagbody repetir-rotacion-pieza
									  
										  (format t "~& ¿Desea rotar la pieza (indicar 0, 90, 180 o 270)?: ")
										  (setf rotacion (read))
										  
										  (loop while t ;Valida rotacion elegida.
											if (not (or (eq rotacion 0) (eq rotacion 90) (eq rotacion 180) (eq rotacion 270)))
												do (format t "~& La rotacion elegida no existe, por favor indique otra: ")
												   (setf rotacion (read))
												   
											else
											   do (loop-finish))
											   
										  (setf pieza-transformada (selecciona-rotacion-pieza pieza-seleccionada rotacion))
										  (format t "~& La pieza rotada queda así: ~&~%")
										  (pinta-pieza-transformada pieza-transformada jugador)
										  
										  ;Pregunta para continuar o repetir seleccion.
										  (format t "~& Si está seguro de su selección indique (si) para continuar o (no) para seleccionar otra: ")
										  (setf continuar (read))
										  
										  (loop while t
											if (and (not (eq continuar 'si)) (not (eq continuar 'no)))
												do (format t "~& La respuesta introducida no es correcta, vuelva a introducir (si) o (no): ")
												   (setf continuar (read))
												   
											else
												if (eq continuar 'si) do (setf continuar nil) (loop-finish)
											
												else do (setf continuar nil) (go repetir-rotacion-pieza)))
									  
									    ;REFLEJO DE PIEZA
									    (tagbody repetir-reflejar-pieza

										  (format t "~& ¿Desea reflejar la pieza (si) o (no)?: ")
										  (setf reflejar (read))
										  
										  (loop while t ;Valida eleccion de reflejo
											if (not (or (eq reflejar 'si) (eq reflejar 'no)))
												do (format t "~& La respuesta introducida no es correcta, vuelva a introducir (si) o (no): ")
												   (setf reflejar (read))
												   
											else
											   do (loop-finish))
											   
										  (setf pieza-transformada (selecciona-reflejo-pieza pieza-transformada reflejar))
										  (format t "~& La pieza final queda así: ~&~%")
										  (pinta-pieza-transformada pieza-transformada jugador)
										  
										  ;Pregunta para continuar o repetir seleccion.
										  (format t "~& Si está seguro de su selección indique (si) para continuar o (no) para seleccionar otra: ")
										  (setf continuar (read))
										  
										  (loop while t
											if (and (not (eq continuar 'si)) (not (eq continuar 'no)))
												do (format t "~& La respuesta introducida no es correcta, vuelva a introducir (si) o (no): ")
												   (setf continuar (read))
												   
											else
												if (eq continuar 'si) do (setf continuar nil) (loop-finish)
											
												else do (setf continuar nil) (go repetir-reflejar-pieza)))
										
										;SELECCION DE POSICION
									    (tagbody repetir-seleccion-posicion
											
										  (format t "~&~%")
										  (pinta-tablero (estado-tablero estado-actual))
										  
										  (format t "~& Indique la fila en que se hubicará la pieza: ")
										  (setf fila (read))
										  (format t "~& Indique la columna en que se hubicará la pieza: ")
										  (setf columna (read))
										  
										  (loop while t ;Valida posicion introducida.
											if (not (and (numberp fila) (numberp columna)))
												do (format t "~& LOS VALORES INTRODUCIDOS NO SON NÚMEROS ~&~%")
												   (format t "~& Indique la fila en que se hubicará la pieza: ")
												   (setf fila (read))
												   (format t "~& Indique la columna en que se hubicará la pieza: ")
												   (setf columna (read))

											else ;Los valores introducidos son numeros.
												if (or (< fila 1) (> fila (first dim-tablero))
													   (< columna 1) (> columna (second dim-tablero)))
															do (format t "~& LOS VALORES INTRODUCIDOS SE SALEN DEL TABLERO ~&~%")
															   (format t "~& Indique la fila en que se hubicará la pieza: ")
															   (setf fila (read))
															   (format t "~& Indique la columna en que se hubicará la pieza: ")
															   (setf columna (read))

												else
															do (loop-finish))
															
										    ;Pregunta para continuar o repetir seleccion.
										    (format t "~& Si está seguro de su selección indique (si) para continuar o (no) para seleccionar otra: ")
										    (setf continuar (read))
										  
										    (loop while t
											  if (and (not (eq continuar 'si)) (not (eq continuar 'no)))
												do (format t "~& La respuesta introducida no es correcta, vuelva a introducir (si) o (no): ")
												   (setf continuar (read))
												   
											  else
												if (eq continuar 'si) do (setf continuar nil) (loop-finish)
											
												else do (setf continuar nil) (go repetir-seleccion-posicion))
												
											;APLICACION DEL MOVIMIENTO
											(if (eq 21 (length (gethash jugador (estado-jugadores estado-actual)))) ;Movimiento inicial.
												(setf inicial t)
												(setf inicial nil))
												
											(setf estado-temporal 
												(aplica-movimiento
													(construye-movimiento pieza-seleccionada pieza-transformada (list fila columna) jugador inicial)
													 estado-actual))
													 
											(if (eq estado-temporal 'no-aplicable)
												(progn 
													(format t "~& LA SELECCIÓN NO CUMPLE LAS REGLAS PARA PODER APLICARLA. ~&~%")
													
													;Pregunta para volver a elegir posicion o volver a seleccionar pieza.
													(format t "~& ¿Quiere elegir otra posicion? (si) o (no) para volver a seleccionar pieza: ")
													(setf continuar (read))
													
													(loop while t
													  if (and (not (eq continuar 'si)) (not (eq continuar 'no)))
														do (format t "~& La respuesta introducida no es correcta, vuelva a introducir (si) o (no): ")
														   (setf continuar (read))
												   
													  else
														if (eq continuar 'si) do (setf continuar nil) (go repetir-seleccion-posicion)
											
														else do (setf continuar nil) (go repetir-seleccion-completa)))))

												
										;VISTA PREVIA Y CONFIRMACION
										(format t "~& El tablero queda de la siguiente forma: ~&~%")												
									    (pinta-tablero (estado-tablero estado-temporal))
										
										;Pregunta para continuar o repetir seleccion.
										(format t "~& ¿Está seguro de que quiere aplicar el movimiento (si) o (no)?: ")
										(setf continuar (read))
										  
										(loop while t
										  if (and (not (eq continuar 'si)) (not (eq continuar 'no)))
											do (format t "~& La respuesta introducida no es correcta, vuelva a introducir (si) o (no): ")
											   (setf continuar (read))
											   
										  else
											if (eq continuar 'si) do (setf continuar nil) (loop-finish)
										
											else do (setf continuar nil) (go repetir-seleccion-completa)))
										
										;APLICA EL MOVIMIENTO DE MANERA DEFINITIVA
										(setf estado-actual estado-temporal)
										
										;INICIALIZA DE NUEVO LAS VARIABLES
										(setf estado-temporal nil)
										(setf nombre-pieza-seleccionada nil)
										(setf pieza-seleccionada nil)
										(setf pieza-transformada nil)
										(setf rotacion nil)
										(setf reflejar nil)
										(setf fila nil)
										(setf columna nil)
										(setf inicial nil)
										(setf turno-actual jugador))

							 ;Jugadores Artificiales. Para variar opciones de la IA modificar parametros.
							 
							 (2 ;EGOISTA
								(progn 
									(format t "~& Le toca mover al jugador ~a (~a). Jugador Artificial Egoista. Espere por favor...~&~%" 
																													jugador (color-jugador jugador))
									(setf estado-actual (first (decide-movimiento (construye-nodo estado-actual jugador) 1 -1 (tipo-jugador EGOISTA))))
									(pinta-tablero (estado-tablero estado-actual))
									(setf turno-actual jugador)
									(format t "~& Introduzca cualquier valor para continuar... ")
									(read)))
							 
							 (3 ;PARANOICO
								(progn 
									(format t "~& Le toca mover al jugador ~a (~a). Jugador Artificial Paranóico. Espere por favor...~&~%" 
																													jugador (color-jugador jugador))
									(setf estado-actual (first (decide-movimiento (construye-nodo estado-actual jugador) 1 -1 (tipo-jugador PARANOICO))))
									(pinta-tablero (estado-tablero estado-actual))
									(setf turno-actual jugador)
									(format t "~& Introduzca cualquier valor para continuar... " )
									(read)))
								 
							 (4 ;ALEATORIO
								(progn 
									(format t "~& Le toca mover al jugador ~a (~a). Jugador Artificial Aleatorio. Espere por favor...~&~%" 
																													jugador (color-jugador jugador))						 
									(setf estado-actual (first (decide-movimiento (construye-nodo estado-actual jugador) 1 -1 (tipo-jugador ALEATORIO))))
									(pinta-tablero (estado-tablero estado-actual))
									(setf turno-actual jugador)
									(format t "~& Introduzca cualquier valor para continuar... " )
									(read))))))
		
		;MENSAJE DE FIN DE JUEGO
		(format t "~& EL JUEGO HA TERMINADO. ~&~%")
		(pinta-tablero (estado-tablero estado-actual))
		
		(loop for jugador from 1 to *numero-jugadores* ;Se ha llegado al estado final.
			if (es-estado-ganador estado-actual turno-actual jugador)
			    do (format t "~& HA GANADO EL JUGADOR ~a (~a) con ~d PUNTOS!!!" jugador (color-jugador jugador) 
									(puntuacion-jugador (evaluacion-estatica estado-actual turno-actual) jugador))))
							 
)


;Funciones auxiliares.

(defun tipo-jugador (identificador)
	"devulve el nombre del tipo de jugador a partir del jugador dado"
	(case identificador
		 (HUMANO 	'jugador-humano)
		 (EGOISTA 	'jugador-egoista)
		 (PARANOICO 'jugador-paranoico)
		 (ALEATORIO 'jugador-aleatorio))
		 
)

(defun inicializa-variables-globales ()
	"asigna los valores necesarios para comenzar a jugar"
	
	(lee-numero-jugadores)
	
	(lee-tipo-jugadores)
	
	(crea-estado-inicial)
	
	(crea-maxima-suma)
	
)

(defun lee-numero-jugadores ()
	"pregunta al usuario cuantos jugadores van a jugar"
	
	(format t "~&~%~% Por favor, introduce el número de jugadores para la partida (2-4): ")
	
	(setf *numero-jugadores* (read))
	
	(loop while t ;Valida informacion introducida.
	
		if (or (< *numero-jugadores* 2) (> *numero-jugadores* 4))
			do (format t "~& El numero de jugadores debe estar entre 2 y 4. Vuelva a introducirlo: ") 
			   (setf *numero-jugadores* (read))
	
		else
		    do (return-from lee-numero-jugadores *numero-jugadores*))
	
)

(defun lee-tipo-jugadores ()
	"pregunta al usuario que tipos de jugadores jugaran"
	
	(format t "~& Seleccione, de entre las siguientes posibilidades, qué tipo de jugador desea para cada uno de los jugadores de la partida: 
		   ~&  - 1.Jugador Humano
		   ~&  - 2.Jugador Artificial (Egoista)
		   ~&  - 3.Jugador Artificial (Paranoico)
		   ~&  - 4.Jugador Artificial (Aleatorio)
		   ~&  NOTA: Indique el valor asociado a cada tipo de jugador (1 para jugador humano, 2 para jugador a. egoista, etc... ~&~%")
			   
							
	(setf *tipos-jugadores* 
		 
		  (let ((tipo nil))
			   
			   (loop for jugador from 1 to *numero-jugadores*
				 do (format t " Jugador ~a: " jugador)
					(setf tipo (read))
					
					(loop while t ;Valida informacion introducida.
					
					  if (or (< tipo 1) (> tipo 4))
						  do (format t "~& Debe seleccionar un tipo correcto. Vuelva a introducirlo: ") 
							 (setf tipo (read))
					
					  else
						  do (loop-finish))
						  
				collect tipo into tipos-jugadores
						
				finally (return tipos-jugadores))))
	
)

(defun crea-estado-inicial ()
	"genera el estado inicial para el juego actual"
	(setf *estado-inicial* (make-estado))
	
	(loop for jugador from 1 to *numero-jugadores*
		do (setf (gethash jugador (estado-jugadores *estado-inicial*)) PIEZAS))
		
)

(defun crea-maxima-suma ()
	"genera la maxima suma para las puntuaciones del juego"
	(setf *maxima-suma* (+ (* 89 *numero-jugadores*) 20))
	
)

(defun pinta-piezas-jugador (piezas-jugador jugador)
	"representa las piezas disponibles para el jugador dado"
	(loop for pieza in piezas-jugador
		do (pinta-pieza pieza jugador))
																						
	(format t " PIEZAS ACTUALES DEL JUGADOR: ~d~% " (length piezas-jugador))

)
	
(defun pinta-tablero (tablero-actual)
	"representa en pantalla el tablero del juego en el estado actual"
	(format t " TABLERO ACTUAL: ~&~%~8t")

	(loop for j from 0 to (1- (second dim-tablero)) ;Imprime los indices de las columnas.
		do (format t "~4t~2a" (+ j 1))
		
		finally (format t "~%"))
	
	(loop for i from 0 to (1- (first dim-tablero))
	   do (format t "~4t~2a : " (+ i 1)) 		    ;Imprime los indices de las filas.
	   
		  (loop for j from 0 to (1- (second dim-tablero))
				do (format t "~a~2t" (color-jugador (aref tablero-actual i j))) ;Imprime las casillas del tablero.
				
				finally (format t "~&"))
				
	   finally (format t "~&~%"))
						  
)

(defun pinta-pieza (pieza jugador)
	"representa en pantalla la pieza dada para el jugador dado"
	(let ((color (color-jugador jugador)))
	     
		 (cond ((equalp pieza I1) (format t "~& PIEZA I1: ~a~&~%" color))
			   ((equalp pieza I2) (format t "~& PIEZA I2: ~a~&~11t~a~&~%" color color))	
			   ((equalp pieza I3) (format t "~& PIEZA I3: ~a~&~11t~a~&~11t~a~&~%" color color color))
			   ((equalp pieza V3) (format t "~& PIEZA V3: ~a~&~11t~a~t~a~&~%" color color color))																	  
			   ((equalp pieza I4) (format t "~& PIEZA I4: ~a~&~11t~a~&~11t~a~&~11t~a~&~%" color color color color))	
			   ((equalp pieza O4) (format t "~& PIEZA O4: ~a~t~a~&~11t~a~t~a~&~%" color color color color))
			   ((equalp pieza T4) (format t "~& PIEZA T4: ~a~t~a~t~a~&~14t~a~&~%" color color color color))																				  
			   ((equalp pieza L4) (format t "~& PIEZA L4: ~a~&~11t~a~&~11t~a~t~a~&~%" color color color color))
			   ((equalp pieza Z4) (format t "~& PIEZA Z4: ~a~t~a~&~14t~a~t~a~&~%" color color color color))			   
			   ((equalp pieza I5) (format t "~& PIEZA I5: ~a~&~11t~a~&~11t~a~&~11t~a~&~11t~a~&~%" color color color color color))
			   ((equalp pieza Z5) (format t "~& PIEZA Z5: ~a~t~a~&~14t~a~&~14t~a~t~a~&~%" color color color color color))
			   ((equalp pieza L5) (format t "~& PIEZA L5: ~a~&~11t~a~&~11t~a~&~11t~a~t~a~&~%" color color color color color))
			   ((equalp pieza Y5) (format t "~& PIEZA Y5: ~a~&~11t~a~t~a~&~11t~a~&~11t~a~&~%" color color color color color))
			   ((equalp pieza N5) (format t "~& PIEZA N5: ~a~&~11t~a~&~11t~a~t~a~&~14t~a~&~%" color color color color color))
			   ((equalp pieza P5) (format t "~& PIEZA P5: ~a~t~a~&~11t~a~t~a~&~11t~a~&~%" color color color color color))
			   ((equalp pieza U5) (format t "~& PIEZA U5: ~a~17t~a~&~11t~a~t~a~t~a~&~%" color color color color color))
			   ((equalp pieza V5) (format t "~& PIEZA V5: ~a~&~11t~a~&~11t~a~t~a~t~a~&~%" color color color color color))
			   ((equalp pieza W5) (format t "~& PIEZA W5: ~a~&~11t~a~t~a~&~14t~a~t~a~&~%" color color color color color))
			   ((equalp pieza T5) (format t "~& PIEZA T5: ~a~t~a~t~a~&~14t~a~&~14t~a~&~%" color color color color color))
			   ((equalp pieza F5) (format t "~& PIEZA F5: ~14t~a~t~a~&~11t~a~t~a~&~14t~a~&~%" color color color color color))																						  
			   ((equalp pieza X5) (format t "~& PIEZA X5: ~14t~a~&~11t~a~t~a~t~a~&~14t~a~&~%" color color color color color))))																						  

)

(defun selecciona-pieza (nombre)
	"devuelve la pieza cuyo nombre coincide con el dado"
	(cond ((eq nombre 'I1) I1)
	      ((eq nombre 'I2) I2)	
	      ((eq nombre 'I3) I3)
	      ((eq nombre 'V3) V3)
	      ((eq nombre 'I4) I4)
	      ((eq nombre 'O4) O4)
	      ((eq nombre 'T4) T4)
	      ((eq nombre 'L4) L4)
	      ((eq nombre 'Z4) Z4)
	      ((eq nombre 'I5) I5)
	      ((eq nombre 'Z5) Z5)
	      ((eq nombre 'L5) L5)
	      ((eq nombre 'Y5) Y5)
	      ((eq nombre 'N5) N5)
	      ((eq nombre 'P5) P5)
	      ((eq nombre 'U5) U5)
	      ((eq nombre 'V5) V5)
	      ((eq nombre 'W5) W5)
	      ((eq nombre 'T5) T5)
	      ((eq nombre 'F5) F5)
	      ((eq nombre 'X5) X5))
		  
)
	
(defun selecciona-rotacion-pieza (pieza rotacion)
	"devuelve la rotacion de la pieza dada de acuerdo a la rotacion dada"
	(case rotacion
		(0 	  pieza)
		(90  (rota90 pieza))
		(180 (rota180 pieza))
		(270 (rota270 pieza)))
		
)

	
(defun selecciona-reflejo-pieza (pieza reflejar)
	"devuelve la pieza reflejada"
	(cond ((eq reflejar 'si) (refleja pieza))
		  ((eq reflejar 'no) pieza))
	
)

(defun pinta-pieza-transformada (pieza-transformada jugador)
	"representa en pantall la pieza reflejada"
	(loop for i from 0 to (1- (array-dimension pieza-transformada 0))
	  do (loop for j from 0 to (1- (array-dimension pieza-transformada 1))   
		   if (not (eq (aref pieza-transformada i j) 0))
				do (format t "~4t~a" (color-jugador jugador))
		   else
				do (format t "~4t~c~c" #\space #\space)
		   
		   finally (format t "~&"))
		   
	  finally (format t "~&~%"))
		   
)
