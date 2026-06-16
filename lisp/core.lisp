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
