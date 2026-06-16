(ql:quickload "local-time") ;carga del archivo "localtime" de la libreria 

;; Requerimiento 1
;; ======================================================== 
;; FUNCIÓN: transicion 
;; NATURALEZA: Pura (para los mismos parametros simpre devuelve el mismo resultado) 
;; ESTRATEGIA: Condicional (Implementada mediante el uso del COND) 
;; IMPACTO: No destructiva (no modifica los argumentos)
;; ======================================================== 

(defun transicion-de-Estado (color-actual cambiar-a)
	(cond
		((and (eq color-actual 'en-rojo) (eq cambiar-a 'verde)) (list color-actual '--> 'cambiar-a-verde))
		((and (eq color-actual 'en-verde) (eq cambiar-a 'amarillo)) (list color-actual '--> 'cambiar-a-amarillo))
		((and (eq color-actual 'en-amarillo) (eq cambiar-a 'rojo)) (list color-actual '--> 'cambiar-a-rojo))
		(t (list color-actual 'accion-por-defecto))
	)
)
;; ======================================================== 
;; FUNCIÓN: segundoExacto
;; NATURALEZA: Pura (su salida dependen unicamente unix. Utiliza operaciones aritmetricas)
;; ESTRATEGIA: aritmetrica (calcula es segundo exacto dentro del ciclo del semaforo, ademas combierte el tiemo universar a unix) 
;; IMPACTO: No destructivas (no destruye el argumento "unix")
;; ========================================================

(defun segundoExacto (unix) ; evuelve el segundo exacto del semaforo
	(mod (- unix 2208988800) 216); el 216 es el ciclo completo del semaforo
)
;; Requerimiento 2
;; ======================================================== 
;; FUNCIÓN: Tiempo-color
;; NATURALEZA: Pura (Implementada mediante el uso de COND y operadores
;; relacionales para determinar el color correspondiente.) 
;; ESTRATEGIA: Condicional (Implementada mediante el uso del COND) 
;; IMPACTO: No destructiva (no modifica los argumentos)
;; ======================================================== 

(defun tiempo-color (tiempo)
	(cond 
		((and (>= (segundoExacto tiempo) 0) (<= (segundoExacto tiempo)  89)) 'ROJO)
		((and (>= (segundoExacto tiempo) 90) (<= (segundoExacto tiempo) 95)) 'AMARILLO)
		((and (>= (segundoExacto tiempo) 96) (<= (segundoExacto tiempo) 215)) 'VERDE)
	)
)
;; Requerimiento 3
;; ======================================================== 
;; FUNCIÓN: logging
;; NATURALEZA: Impura (produce salida mediante el format)
;; ESTRATEGIA: Condicional (Implementada con el uso del COND para la evaluacion del estado del semaforo devuelto por la funcion "tiempo-color")
;; IMPACTO: No destructivas (no modifica argumento)
;; ========================================================

(defun analisis-ciclos (tiempo) ; forma del tiempo epoch
	(cond 
		((equal (symbol-name (tiempo-color tiempo)) "VERDE") (format nil "Tiempo <~a>: La luz a cambiado de rojo a verde" (formato-localtime tiempo)))
		((equal (symbol-name (tiempo-color tiempo)) "AMARILLO") (format nil "Tiempo <~a>: La luz a cambiado de verde a amarillo " (formato-localtime tiempo)))
		((equal (symbol-name (tiempo-color tiempo)) "ROJO") (format nil "Tiempo <~a>: La luz a cambiado de amarrilo a rojo" (formato-localtime tiempo)))
	)
)

;; FASE 2: integracion de la libreria local-time
;; ========================================================
;; FUNCIÓN: formato-localtime
;; NATURALEZA: Pura (mismo epoch siempre devuelve el mismo string)
;; ESTRATEGIA: Composicion de funciones (local-time:unix-to-timestamp + local-time:format-timestring)
;; IMPACTO: No destructiva (no modifica el argumento epoch)
;; ========================================================

(defun formato-localtime (epoch)
	(local-time:format-timestring nil 
		(local-time:unix-to-timestamp (- epoch 2208988800)) :format '("[" (:year 4) "-" (:month 2) "-" (:day 2) " " (:hour 2) ":" (:min 2) ":" (:sec 2) "]"))

)

;; Requerimiento 4a
;; ======================================================== 
;; FUNCIÓN:	Duracion-ciclo
;; NATURALEZA: Pura (Dados los mismos tiempos, siempre calcula y retorna la misma lista)
;; ESTRATEGIA: Condicional y operaciones aritmetricas basicas
;; IMPACTO: No destructiva
;; ========================================================

(defun duracion-ciclo (t-rojo t-amarillo t-verde); en ambos casos retorna una lista con el total de segundos y con un simbolo si es optimo o no

	(let ((total (+ t-rojo t-amarillo t-verde)))
		(if (and (>= total 35) (<= total 150))
			(list total 'Rango-optimo)
			(list total 'fuera-de-rango)
		)
	)
)

;; Requerimiento 4b
;; ======================================================== 
;; FUNCIÓN: recomendacion-ciclo
;; NATURALEZA: Pura (para la misma lista de entrada, el texto de retorno no cambia)
;; ESTRATEGIA: Condicional mediante COND 
;; IMPACTO: No destructiva
;; ========================================================

(defun recomendacion-ciclo (totalCiclo); recibe como parametro la lista retornada por la funcion "duracion-ciclo"
	(cond 
		((< (first totalCiclo) 35) "El ciclo es bajo, se recomienda uno que este dentro de 35-150")
		((> (first totalCiclo) 150) "El ciclo es demaciado alto, se recomienda uno que este dentro de 35-150")
		(t "El ciclo es optimo para estanderes de ingenieria de trafico")
	)
)

;; Requerimiento 5
;; ======================================================== 
;; FUNCIÓN: ciclos-por-tiempo
;; NATURALEZA: Pura (el calculo matematico es determinista y libres de efectos secundarios)
;; ESTRATEGIA: Composicion de funciones y operaciones aritmetricas (llamda a la funcion "duracion-ciclo") 
;; IMPACTO:
;; ========================================================

(defun ciclos-por-tiempo (minutos t-rojo t-amarillo t-verde)
	(float (/ (* minutos 60) (first (duracion-ciclo t-rojo t-amarillo t-verde))))
)

;; Requerimiento 6
;; ======================================================== 
;; FUNCIÓN: porcentaje-porColor
;; NATURALEZA: Pura (el cálculo matemático es determinista y libre de efectos secundarios)
;; ESTRATEGIA: Composición de funciones y operaciones aritméticas (llamada a la función "duración-ciclo")
;; IMPACTO:	No destrucitvas (devuelve el mismo valor para los mismos parametros)
;; ========================================================

(defun porcentaje-porColor (t-rojo t-amarillo t-verde); se pasa los tres timepos y la lista retornada por la funcion duracion ciclo
	(let ((ciclo (duracion-ciclo t-rojo t-amarillo t-verde)))
		(list 
			(list 'rojo (float (* (/ t-rojo (first ciclo)) 100)))
			(list 'amarillo (float (* (/ t-amarillo (first ciclo)) 100)))
			(list 'verde (float (* (/ t-verde (first ciclo)) 100))); mientras los segundos de los colores no cambien con el tiempo el porcentaje
		)														   ; sigue siendo el mismo sin importar las horas
	)
)
