;El siguiente codigo define los elementos necesarios para el juego NIM.
;El NIM consiste en un numero de fichas sobre un tablero de manera que cada jugador, en su turno, puede coger 1, 2 o 3 fichas.
; Pierde aquel jugador en cuyo turno no tenga fichas que coger.

;Definicion de variables globales

(setf *estado-inicial* 4)
(setf *movimientos* '(quitar-uno quitar-dos quitar-tres))
(setf *minimo-valor* -1)
(setf *maximo-valor* 1)
(setf *max* 'max)
(setf *min* 'min)

;Funciones necesarias para la representación del juego

(defun es-estado-final (estado)
  "indentifica estado final"
  (if (= estado 0) T NIL)
)

(defun es-estado-ganador (estado turno jugador)
  "identifica estado ganador para el jugador dado"
  (if (and (es-estado-final estado) (eq turno jugador)) T NIL)
)

(defun aplica-movimiento (movimiento estado)
  "genera el estado sucesor para cada posible movimiento del juego"
  (funcall movimiento estado)
)

(defun evaluacion-estatica (estado turno)
  "valora un nodo a partir del estado y el turno de movimiento en ese estado"
  (cond ((es-estado-ganador estado turno 'max) 1)
        ((es-estado-ganador estado turno 'min) -1)
        (t 0)
  )
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
  
    


