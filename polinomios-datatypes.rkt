#lang eopl
;Autores: Nombre1 Codigo1, Nombre2 Codigo2

;; Taller 1 — Polinomios dispersos.
;; Parte 3: representación con datatypes.
;;
;; Interfaz del TAD. Cada función va comentada con su nombre, su contrato
;; (entrada -> salida) y su propósito, y ninguna recorre la lista de términos
;; más de una vez ni la ordena al final.
;;
;;   polinomio-cero    : symbol -> polinomio
;;   insertar-termino  : polinomio x coeficiente x exponente -> polinomio
;;   coeficiente-de    : polinomio x exponente -> coeficiente
;;   eliminar-termino  : polinomio x exponente -> polinomio
;;   sumar             : polinomio x polinomio -> polinomio

;;Interfaz

(define-datatype polinomio polinomio?
  (poli (var variable?)
        (terms terminos?)))

(define-datatype variable variable?
  (nombre-var (s symbol?))
  )

(define-datatype terminos terminos?
  (sin-terminos)
  (mas-terminos (term termino?)
                (resto terminos?))
  )

(define-datatype termino-tad termino?
  (termino (coef coeficiente?)
           (expo exponente?))
       )

(define-datatype coeficiente coeficiente?
  (coef-ent (n integer?))
  (coef-rac (num integer?)
            (den integer?))
  )

(define-datatype exponente exponente?
  (expo-nat (k integer?))
  )


;;Funciones Auxiliares

;;Nombre de la funcion: coef-abstracto->concreto
;;Contrato(input and output): le entra un coeficiente abstracto(termino->coef term)
;;y devuelve el coeficiente concreto es decir un coeficiente entero o raciona(numerador/denominador)en este caso
;;Proposito: poder convertir un termino abstracto a un termino concreto con el objetivo de mostrar la solucion en forma concreta
;;y tambien se creo la funcion con el proposito de segmentar mejor el codigo y no dejar todo el codigo en una sola funcion.
(define coef-abstracto->concreto
  (lambda (exp)
    (cases coeficiente exp
      (coef-ent(n) n)
       (coef-rac (num den)
                 (/ num den))
      )))

;;Esta funcion no se le puede meter cases ya que directamente recibe un dato concreto por lo tanto en este caso no aplicaria el uso de cases que cumplen la tarea de extractores
;;Nombre de la funcion: coef-concreto->abstracto
;;Contrato(input and output): le entra un coeficiente concreto ejemplo un 5 y debe devolver un coeficiente abstracto
;;respetando su tipo de representacion si es entero o racional. ejemplo de salida (coef-ent 5)
;;Proposito: poder convertir un coeficiente concreto a un coeficiente abstracto con su respectiva representacion
(define coef-concreto->abstracto
  (lambda (exp)           
    (cond ;numerator denominator
      [(integer? exp) (coef-ent exp)]
      [(rational? exp) (coef-rac (numerator exp)
                                 (denominator exp))]
      [else (eopl:error
             'coef-abstracto->concreto "coeficiente invalido")]
      )
    ))

;;Nombre de la funcion: buscar-coeficiente
;;Contrato(input and output): le entra los terminos del polinomio establecido y el exponente establecido por el usuario
;;y devuelve el coeficiente asociado al exponente establecido por el usuario.En
;;caso de que no exista el exponente en el polinomio se devuelve un error respecto a que ese exponente no esta asociado a ningun coeficiente
;;Proposito: poder buscar el coeficiente asociado al exponente ingresado. Su otro proposito es que se creo para trabajar directamente con los terminos
;;y exponente del polinomio ya de forma organizada y separada .Dejando la funcion principal coeficiente-de mas limpia
(define buscar-coeficiente
  (lambda (terms expo)
    (cases terminos terms
      (sin-terminos () (eopl:error
       "Lo siento: el polinomio
       no tiene termino con ese exponete ingresado"))
      (mas-terminos (term resto)
       (cases termino-tad term  ;;Obtiene termino
         (termino (coef expt)
           (cases exponente expt ;;Obtiene exponente
             (expo-nat(k)
               (cond
                 [(= k expo)
                  (coef-abstracto->concreto coef)]
                 [(> expo k) (eopl:error 'buscar-coeficiente
       "Lo siento: el polinomio
       no tiene termino con ese exponete ingresado")]
                 [else
                  (buscar-coeficiente resto expo)]
                ))))
           )))
    ))

;;Nombre de la funcion: buscar-eliminar
;;Contrato(input and output): le entra los terminos del polinomio y el exponente del termino a eliminar
;;y devuelve un polinomio nuevo sin ese termino borrado
;;Proposito: Poder buscar donde esta el termino que tiene asociado el exponente a borrar y posteriormente borrarlo para
;;devolver los terminos del polinomio sin ese termino que se solicito borrar por medio del exponente
(define buscar-eliminar
  (lambda(terms expo)
    (cases terminos terms
      (sin-terminos () (eopl:error
       "Lo siento: el polinomio
       no tiene termino con ese exponete ingresado por lo
       tanto no podemos eliminar termino"))
      (mas-terminos (term resto)
        (cases termino-tad term
          (termino (coef expt)
            (cases exponente expt
              (expo-nat (k)
                (cond
                  [(= k expo) resto]
                  [(> expo k) (eopl:error 'buscar-eliminar
       "Lo siento: el polinomio
       no tiene termino con ese
       exponente ingresado por lo tanto no podemos eliminar termino")]
                  [else (mas-terminos term
                              (buscar-eliminar resto expo))]
                  )))
            ))))
    ))

;;Area del Programador
(provide polinomio-cero insertar-termino coeficiente-de eliminar-termino sumar)

;;Funcion que no se le aplica cases debido a que no extrae ningun tipo de dato solo construye y cases se necesita es para poder extraer los datos y maniobrarlos
;;Nombre de la funcion: polinomio-cero
;;Contrato(input and output): le entra un simbolo ejemplo 'x
;;y devuelve un polinomio abstracto con su nombre de variable indicada y sin termino en la parte de terminos
;;Proposito: Poder construir un polinomio sin terminos en la variable dada a construir
(define polinomio-cero
  (lambda (variable)  
    (cond
      [(symbol? variable);Validar que en efecto la variable sea un simbolo
       (poli(nombre-var variable) (sin-terminos))]
      [else (eopl:error "la variable debe ser un simbolo")]
    )))


(define insertar-termino
  (lambda (polinomio coeficiente exponente)
    (eopl:error 'insertar-termino "Sin implementar")))

;;Nombre de la funcion: coeficiente-de
;;Contrato(input and output): le entra un polinomio y un exponente
;;y devuelve un coeficiente del polinomio asociado al exponente establecido
;;Proposito: Poder obtener adecuadamente el coeficiente que esta asociado ala potencia ingresada anterior mente
;;ejecutandose la logica pesada en la funcion buscar-coeficiente
(define coeficiente-de
  (lambda (p exponente)
    (cases polinomio p
      (poli (var terms)
        (buscar-coeficiente terms exponente))
      )))

;;Nombre de la funcion: eliminar-termino
;;Contrato(input and output): le entra un polinomio y el exponente del termino a borrar 
;;y devuelve un nuevo polinomio abstracto sin ese termino 
;;Proposito: Poder eliminar un termino del polinomio por medio de un exponente que indica que termino borrar junto con el exponente fue establecido
(define eliminar-termino
  (lambda (p exponente)
    (cases polinomio p
      (poli (var terms)
        (buscar-eliminar terms exponente))
      )
    ))

(define sumar
  (lambda (p q)
    (eopl:error 'sumar "Sin implementar")))

;;Ejemplo poli  p=4x^5+(-3/2)x^2+7 para hacer pruebas
(define p (poli (nombre-var 'x) (mas-terminos
 (termino(coef-ent 4) (expo-nat 5))
 (mas-terminos(termino (coef-rac -3 2) (expo-nat 2))
   (mas-terminos(termino (coef-ent 7)
     (expo-nat 0)) (sin-terminos)
              )))))