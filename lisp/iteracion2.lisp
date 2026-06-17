;;================
;;ITERACION 2
;;================

(defun segundoExacto (unix)
	(mod (- unix 2208988800) 225)
)

(defun timer (tiempo)
	(cond
		((and (>= (segundoExacto tiempo) 0)
		      (<= (segundoExacto tiempo) 89))
		 'ROJO)

		((and (>= (segundoExacto tiempo) 90)
		      (<= (segundoExacto tiempo) 92))
		 'AMARILLO-INTERMITENTE)

		((and (>= (segundoExacto tiempo) 93)
		      (<= (segundoExacto tiempo) 98))
		 'AMARILLO)

		((and (>= (segundoExacto tiempo) 99)
		      (<= (segundoExacto tiempo) 101))
		 'ROJO-INTERMITENTE)

		((and (>= (segundoExacto tiempo) 102)
		      (<= (segundoExacto tiempo) 221))
		 'VERDE)

		((and (>= (segundoExacto tiempo) 222)
		      (<= (segundoExacto tiempo) 224))
		 'AMARILLO-INTERMITENTE)
	)
)

(defun transicion2 (color-actual cambiar-a)
	(cond

		((and (eq color-actual 'en-rojo)
		      (eq cambiar-a 'amarillo-intermitente))
		 (list color-actual '--> 'amarillo-intermitente))

		((and (eq color-actual 'amarillo-intermitente)
		      (eq cambiar-a 'amarillo))
		 (list color-actual '--> 'amarillo))

		((and (eq color-actual 'amarillo)
		      (eq cambiar-a 'rojo-intermitente))
		 (list color-actual '--> 'rojo-intermitente))

		((and (eq color-actual 'rojo-intermitente)
		      (eq cambiar-a 'verde))
		 (list color-actual '--> 'verde))

		(t
		 (list color-actual 'accion-por-defecto))
	)
)

(defun logging (tiempo)

	(cond

		((eq (tiempo-color tiempo) 'VERDE)
		 (format nil
		         "Tiempo <~a>: Cambio a VERDE"
		         (formato-localtime tiempo)))

		((eq (tiempo-color tiempo) 'AMARILLO)
		 (format nil
		         "Tiempo <~a>: Cambio a AMARILLO"
		         (formato-localtime tiempo)))

		((eq (tiempo-color tiempo) 'ROJO)
		 (format nil
		         "Tiempo <~a>: Cambio a ROJO"
		         (formato-localtime tiempo)))

		((eq (tiempo-color tiempo) 'AMARILLO-INTERMITENTE)
		 (format nil
		         "Tiempo <~a>: AMARILLO INTERMITENTE"
		         (formato-localtime tiempo)))

		((eq (tiempo-color tiempo) 'ROJO-INTERMITENTE)
		 (format nil
		         "Tiempo <~a>: ROJO INTERMITENTE"
		         (formato-localtime tiempo)))

)

(defun informe (datos)

	(with-open-file
		(stream
		 "informe-ejecucion-semaforo.txt"
		 :direction :output
		 :if-exists :supersede)

		(format stream
		        "Informe de Ejecución del Sistema Semafórico~%")

		(format stream
		        "=========================================%%")

		(mapcar
			(lambda (dato)
				(format stream "a%" dato))
			datos)

		(format stream
		        "~%--- Fin del Informe ---")
	)
)
