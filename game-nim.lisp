;Implementacion de NIM para multipler jugadores.

;La variante del NIM utilizada consiste en varios grupos de fichas (el numero de jugadores determina el numero de grupos) con un numero aleatorio
; de fichas en cada grupo. Cada jugador, en su turno, solo puede coger fichas de un grupo. Puede coger desde una ficha, como minimo, hasta todas las
; del grupo de fichas elegido. El jugador que coge la ultima ficha pierde todas sus fichas quedando con 0 y por tanto, pierde el juego. El jugador al 
; que le toca mover inmediatamente despues del perdedor recibe todas las fichas de este. Finalmente gana el que mas fichas tenga.

;Funciones auxiliares

(defun construye-nodo (estado-actual turno-actual)
	"devuelve un nodo del juego formado a partir del estado y el turno dado"
	(list estado-actual turno-actual)
	
)

(defun construye-estado (fpendientes fjugadores)
	"devuelve un estado del juego formado a partir de las fichas pendientes y las fichas de los jugadores dadas"
	(list fpendientes fjugadores)
	
)

;Funciones de inicializacion

(defun genera-estado-inicial (numero-jugadores)
	"genera un estado inicial a partir del numero de jugadores dado"
	
	(construye-estado 
	
		(loop for i from 1 to numero-jugadores append (list (random (* numero-jugadores numero-jugadores))))
								
		(loop for i from 1 to numero-jugadores collect 0))
			
)

(defun calcula-maximo-valor (estado-inicial numero-jugadores)
	"devuelve la puntuacion maxima para un jugador dado un estado inicial y el numero de jugadores"
	(let ((fpendientes (fichas-pendientes estado-inicial)))
	
		(loop for grupo from 1 to (length fpendientes)
			sum (nth (1- grupo) fpendientes) into numero-fichas
			finally (return (- numero-fichas (1- numero-jugadores))))) ;Uno de los jugadores coge al menos el numero-jugadores menos 1 fichas.
			
)

(defun calcula-maxima-suma (estado-inicial)
	"devuelve la maxima suma de las puntuaciones de todos los jugadores dado un estado inicial"
	(let ((fpendientes (fichas-pendientes estado-inicial)))
	
		(loop for grupo from 1 to (length fpendientes)
			sum (nth (1- grupo) fpendientes)))
			
)

;Funciones de accesso a los elementos de un nodo del juego.

;Un nodo del juego esta formado por una lista con un elemento que representa el estado y otro el turno. El estado consiste en una lista con las fichas
; que quedan por coger de cada grupo y el numero de fichas que ha cogido cada jugador.

(defun estado (nodo)
	"devuelve el estado del juego en el nodo dado"
	(first nodo)
	
)

(defun fichas-pendientes (estado-actual)
	"devuelve el numero de fichas que quedan por coger de cada grupo"
	(first estado-actual)
	
)

(defun fichas-jugadores (estado-actual)
	"devuelve el numero de fichas que ha cogido cada jugador"
	(second estado-actual)
	
)

(defun turno (nodo)
	"devuelve el identificador del jugador al que le toca mover"
	(second nodo)
	
)

;Definicion de constantes y variables globales

(defvar *numero-jugadores* 3)

(defvar *estado-inicial* (genera-estado-inicial *numero-jugadores*))
												
(defvar *minimo-valor* 0)
(defvar *maximo-valor* (calcula-maximo-valor *estado-inicial* *numero-jugadores*))
(defvar *maxima-suma*  (calcula-maxima-suma *estado-inicial*))


;Funciones necesarias para la representación del juego

(defun es-estado-final (estado-actual)
	"determina si el estado dado es un estado final del juego"
	(let ((fpendientes (fichas-pendientes estado-actual)))
  
		  (loop for grupo from 1 to (length fpendientes)
		    sum (nth (1- grupo) fpendientes) into numero-fichas
		    finally (if (eq numero-fichas 0) (return t) (return nil))))

)

(defun es-estado-ganador (estado-actual turno-actual jugador)
	"determina si el estado dado es ganador para el jugador dado"
	(let* ((fjugadores (evaluacion-estatica estado-actual turno-actual))
		   (fjugador (nth (1- jugador) fjugadores)))
		   
		  (if (es-estado-final estado-actual)
		   
			  (loop for j from 1 to *numero-jugadores*
			  
				if (and (not (eq j jugador)) (<= fjugador (nth (1- j) fjugadores))) do (return-from es-estado-ganador nil)
				
				finally (return-from es-estado-ganador t))))
				
)

;Un movimiento viene definido por el numero de fichas que va a coger el jugador y el grupo del que lo va a coger.

(defun aplica-movimiento (movimiento estado-actual)
	"genera el estado sucesor para cada posible movimiento del juego"
	(let ((quita-fichas (first movimiento))
		  (quita-grupo (second movimiento))
		  (turno-actual (third movimiento))
		  (fpendientes (fichas-pendientes estado-actual))
		  (fjugadores (fichas-jugadores estado-actual)))
		  
		  
		  (if (or (> quita-grupo (length fpendientes)) (<= quita-grupo 0)
				  (> turno-actual *numero-jugadores*) (<= turno-actual 0))
		  
			'no-aplicable
			
			(if (or (> quita-fichas (nth (1- quita-grupo) fpendientes)) (<= quita-fichas 0))
				
				'no-aplicable
				
				(construye-estado

					(loop for grupo from 1 to (length fpendientes)
						if (eq grupo quita-grupo)
							collect (- (nth (1- grupo) fpendientes) quita-fichas)
						else
							collect (nth (1- grupo) fpendientes))

					(loop for jugador from 1 to *numero-jugadores*
						if (eq jugador turno-actual) 
							collect (+ (nth (1- jugador) fjugadores) quita-fichas)
						else
							collect (nth (1- jugador) fjugadores))))))
		
)

(defun evaluacion-estatica (estado-actual turno-actual)
	"valora un nodo a partir del estado y el turno de movimiento en ese estado"
	(let ((turno-anterior (if (eq turno-actual 1) *numero-jugadores* (1- turno-actual)))
		  (fjugadores (fichas-jugadores estado-actual)))
		  
		 (if (es-estado-final estado-actual)
		 
			 (loop for jugador from 1 to *numero-jugadores*
			 
				if (eq jugador turno-anterior)
					collect 0

				if (eq jugador turno-actual)
					collect (+ (nth (1- jugador) fjugadores) (nth (1- turno-anterior) fjugadores))
					
				if (and (not (eq jugador turno-anterior)) (not (eq jugador turno-actual)))
					collect (nth (1- jugador) fjugadores))
		
			 
			 fjugadores)) ;No es estado final
			 
)

(defun movimientos (estado-actual turno-actual)
	"devuelve la lista de movimientos disponibles"
	(let ((fpendientes (fichas-pendientes estado-actual)))
	
		(loop for grupo from 1 to (length fpendientes)
		  append (loop for x from (nth (1- grupo) fpendientes) downto 1
					 collect (list x grupo turno-actual))))
					 
				 
)