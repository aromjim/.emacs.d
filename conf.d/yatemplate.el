(use-package yatemplate
  :init
  (yatemplate-fill-alist)
  :config
  (setq asignaturas '("Lógica Informática"
		      "Técnicas de Computación para la Estadística"))
  (setq titulaciones '("Grado en Estadística"
		       "Grado en Ingeniería Informática"))
  (setq convocatorias '("diciembre"
			"febrero"
			"junio"
			"septiembre"))
  (setq exámenes '("Primer control"
		   "Segundo control"
		   "Examen")))

(use-package autoinsert
  :init
  (auto-insert-mode 1))
