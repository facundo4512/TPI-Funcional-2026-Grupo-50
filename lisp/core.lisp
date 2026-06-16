(ql:quickload "local-time") ;carga del archivo "localtime" de la libreria 

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
