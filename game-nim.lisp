;Implementacion de NIM para multipler jugadores.

;La variante del NIM utilizada consiste en un numero de fichas sobre un tablero de manera que cada jugador, 
; en su turno, puede coger 1, 2 o 3 fichas. Gana el jugador al que le toca mover despues de que se haya cogido
; la ultima ficha.

;Definicion de constantes y variables globales

(defvar *movimientos* '(quitar-uno quitar-dos quitar-tres))
(defvar *minimo-valor* 0)
(defvar *maximo-valor* 1)
(defvar *numero-jugadores* 3)

;Funciones de accesso a los elementos de un nodo del juego.

;Un nodo del juego esta formado por una lista con el estado (numero de fichas en el tablero) y el turno del jugador al que le toca mover.

(defun estado (nodo)
  "devuelve el estado del juego en el nodo dado"
  (first nodo)
)

(defun turno (nodo)
  "devuelve el identificador del jugador al que le toca mover"
  (second nodo)
)

;Funciones necesarias para la representación del juego

(defun es-estado-final (estado-actual)
  "determina si el estado dado es un estado final del juego"
  (if (= estado-actual 0) T NIL)
)

(defun es-estado-ganador (estado-actual turno-actual jugador)
  "determina si el estado dado es ganador para el jugador dado"
  (if (and (es-estado-final estado-actual) (eq turno-actual jugador)) T NIL)
)

(defun aplica-movimiento (movimiento estado-actual)
  "genera el estado sucesor para cada posible movimiento del juego"
  (funcall movimiento estado-actual)
)

(defun evaluacion-estatica (estado-actual turno-actual)
  "valora un nodo a partir del estado y el turno de movimiento en ese estado"
  (loop for jugador from 1 to *numero-jugadores*
       if (es-estado-ganador estado-actual turno-actual jugador)
           collect 1
       else 
           collect 0)
)

(defun movimientos (estado-actual turno-actual)
	"devuelve la lista de posibles movimientos dado un estado del juego y el turno de movimiento"
	*movimientos*
)

;Funciones que realizan las operaciones asociadas a los movimientos del juego

(defun quitar-uno (estado-actual)
  "quita una ficha de la mesa"
  (if (>= estado-actual 1) (- estado-actual 1) 'no-aplicable)
)

(defun quitar-dos (estado-actual)
  "quita dos fichas de la mesa"
  (if (>= estado-actual 2) (- estado-actual 2) 'no-aplicable)
)

(defun quitar-tres (estado-actual)
  "quita tres fichas de la mesa"
  (if (>= estado-actual 3) (- estado-actual 3) 'no-aplicable)
)

;Funciones auxiliares

(defun construye-nodo (estado-actual turno-actual)
  "devuelve un nodo del juego formado por el estado y el turno dado"
  (list estado-actual turno-actual)
)
  
    


