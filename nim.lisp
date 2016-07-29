;El siguiente codigo define los elementos necesarios para representar el juego NIM para multiples jugadores.

;La variante del NIM utilizada consiste en un numero de fichas sobre un tablero de manera que cada jugador, 
; en su turno, puede coger 1, 2 o 3 fichas. Gana el jugador al que le toca mover despues de que se haya cogido
; la ultima ficha.

;Definicion de variables globales

(defvar *movimientos* '(quitar-uno quitar-dos quitar-tres))
(defvar *minimo-valor* 0)
(defvar *maximo-valor* 1)
(defvar *numero-jugadores* 3)

;Un nodo del juego esta formado por una lista con el estado del mismo y el turno del jugador al que le toca mover.
; Los estados del juego vendran determinados por el numero de fichas en el tablero.
; El turno de cada jugador se indicara mediante un identificador numerico.

;Funciones de accesso a los elementos de un nodo del juego.

(defun estado (nodo)
  "devuelve el estado del juego en el nodo dado, en este caso, el primer elemento de la lista"
  (first nodo)
)

(defun turno (nodo)
  "devuelve el identificador del jugador al que le toca mover, en este caso, el segundo elemento de la lista"
  (second nodo)
)

;Funciones necesarias para la representación del juego

(defun es-estado-final (estado)
  "indentifica estado final"
  (if (= estado 0) T NIL)
)

(defun es-estado-ganador (estado turno jugador)
  "identifica estado ganador para el jugador dado teniendo en cuenta el turno"
  (if (and (es-estado-final estado) (eq turno jugador)) T NIL)
)

(defun aplica-movimiento (movimiento estado)
  "genera el estado sucesor para cada posible movimiento del juego"
  (funcall movimiento estado)
)

(defun evaluacion-estatica (estado turno)
  "valora un nodo a partir del estado y el turno de movimiento en ese estado"
  (loop for jugador from 1 to *numero-jugadores*
       if (es-estado-ganador estado turno jugador)
           collect 1
       else 
           collect 0)
)

;Funciones que realizan las operaciones asociadas a los movimientos del juego

(defun quitar-uno (estado)
  "quita una ficha de la mesa"
  (if (>= estado 1) (- estado 1) 'no-aplicable)
)

(defun quitar-dos (estado)
  "quita dos fichas de la mesa"
  (if (>= estado 2) (- estado 2) 'no-aplicable)
)

(defun quitar-tres (estado)
  "quita tres fichas de la mesa"
  (if (>= estado 3) (- estado 3) 'no-aplicable)
)

;Funciones auxiliares

(defun construye-nodo (estado turno)
  "devuelve un nodo del juego formado por el estado y el turno dados"
  (list estado turno)
)
  
    


