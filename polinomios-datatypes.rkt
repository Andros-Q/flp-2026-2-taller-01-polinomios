#lang eopl
;Autores: Andres Felipe Quiceno gil Codigo:2477362
;Yonier Alejandro Vega Rojas Codigo:2477056
;Jhon Fabricio Hurtado Marin Codigo: 2459472
;Juan Esteban Aguirre Castañeda Codigo:2459676

;; Taller 1 — Polinomios dispersos.
;; Parte 3: representación con datatypes.
;;
;; Interfaz del TAD. Cada función va comentada con su nombre, su contrato
;; (entrada -> salida) y su propósito. La representación mantiene los
;; exponentes en orden descendente, sin exponentes repetidos y sin
;; coeficientes iguales a cero.
;;
;;   polinomio-cero    : símbolo -> polinomio
;;   insertar-termino  : polinomio x coeficiente x exponente -> polinomio
;;   coeficiente-de    : polinomio x exponente -> coeficiente
;;   eliminar-termino  : polinomio x exponente -> polinomio
;;   sumar             : polinomio x polinomio -> polinomio


;; Se exportan las funciones públicas del TAD, los constructores y los
;; predicados de los datatypes para poder usarlos en los ejemplos y en el
;; archivo de pruebas. Las funciones auxiliares permanecen internas.
(provide polinomio-cero
         insertar-termino
         coeficiente-de
         eliminar-termino
         sumar
         poli nombre-var sin-terminos mas-terminos termino
         coef-ent coef-rac expo-nat
         polinomio? variable? terminos? termino-tad?
         coeficiente? exponente?)


;; =========================
;; Interfaz: tipos de datos
;; =========================

;; Nombre del datatype: polinomio
;; Propósito: representar un polinomio mediante su variable y sus términos.
(define-datatype polinomio polinomio?
  (poli (var variable?)
        (terms terminos?)))


;; Nombre del datatype: variable
;; Propósito: representar la variable simbólica del polinomio.
(define-datatype variable variable?
  (nombre-var (s symbol?)))


;; Nombre del datatype: terminos
;; Propósito: representar una lista de términos del polinomio.
;; sin-terminos representa una lista vacía.
;; mas-terminos representa un término seguido del resto de la lista.
(define-datatype terminos terminos?
  (sin-terminos)
  (mas-terminos (term termino-tad?)
                (resto terminos?)))


;; Nombre del datatype: termino-tad
;; Propósito: representar un término mediante un coeficiente y un exponente.
;; El tipo se llama termino-tad porque define-datatype no deja que el nombre
;; del tipo sea igual al de su variante (termino).
(define-datatype termino-tad termino-tad?
  (termino (coef coeficiente?)
           (expo exponente?)))


;; Nombre del datatype: coeficiente
;; Propósito: representar coeficientes enteros o racionales.
;; Los racionales se representan mediante numerador y denominador.
(define-datatype coeficiente coeficiente?
  (coef-ent (n integer?))
  (coef-rac (num integer?)
            (den integer?)))


;; Nombre del datatype: exponente
;; Propósito: representar el exponente de un término.
(define-datatype exponente exponente?
  (expo-nat (k integer?)))


;;Funciones Auxiliares


;;Nombre de la funcion: coef-abstracto->concreto
;;Contrato(input and output): le entra un coeficiente abstracto(termino->coef term)
;;y devuelve el coeficiente concreto es decir un coeficiente entero o racional
;;(numerador/denominador) en este caso
;;Proposito: poder convertir un termino abstracto a un termino concreto con el objetivo de mostrar la solucion en forma concreta
;;y tambien se creo la funcion con el proposito de segmentar mejor el codigo y no dejar todo el codigo en una sola funcion.
(define coef-abstracto->concreto
  (lambda (exp)
    (cases coeficiente exp
      (coef-ent (n)
        n)
      (coef-rac (num den)
        (/ num den)))))


;;Nombre de la funcion: coef-concreto->abstracto
;;Contrato(input and output): le entra un coeficiente concreto ejemplo un 5 y debe devolver un coeficiente abstracto
;;respetando su tipo de representacion si es entero o racional. ejemplo de salida (coef-ent 5)
;;Proposito: poder convertir un coeficiente concreto a un coeficiente abstracto con su respectiva representacion
(define coef-concreto->abstracto
  (lambda (exp)
    (cond
      [(and (number? exp)
            (rational? exp)
            (exact? exp))
       (if (integer? exp)
           (coef-ent exp)
           (coef-rac (numerator exp)
                     (denominator exp)))]
      [else
       (eopl:error
        'coef-concreto->abstracto
        "Coeficiente invalido")])))


;;Nombre de la funcion: buscar-coeficiente
;;Contrato(input and output): le entra los terminos del polinomio establecido y el exponente establecido por el usuario
;;y devuelve el coeficiente asociado al exponente establecido por el usuario.En
;;caso de que no exista el exponente en el polinomio se devuelve un error respecto a que ese exponente no esta asociado a ningun coeficiente
;;Proposito: poder buscar el coeficiente asociado al exponente ingresado. Su otro proposito es que se creo para trabajar directamente con los terminos
;;y exponente del polinomio ya de forma organizada y separada .Dejando la funcion principal coeficiente-de mas limpia
(define buscar-coeficiente
  (lambda (terms expo)
    (cases terminos terms
      (sin-terminos ()
        (eopl:error
         'buscar-coeficiente
         "Lo siento: el polinomio no tiene termino con ese exponente ingresado"))

      (mas-terminos (term resto)
        (cases termino-tad term
          (termino (coef expt)
            (cases exponente expt
              (expo-nat (k)
                (cond
                  [(= k expo)
                   (coef-abstracto->concreto coef)]

                  [(> expo k)
                   (eopl:error
                    'buscar-coeficiente
                    "Lo siento: el polinomio no tiene termino con ese exponente ingresado")]

                  [else
                   (buscar-coeficiente resto expo)])))))))))


;;Nombre de la funcion: buscar-eliminar
;;Contrato(input and output): le entra los terminos del polinomio y el exponente del termino a eliminar
;;y devuelve un polinomio nuevo sin ese termino borrado
;;Proposito: Poder buscar donde esta el termino que tiene asociado el exponente a borrar y posteriormente borrarlo para
;;devolver los terminos del polinomio sin ese termino que se solicito borrar por medio del exponente
(define buscar-eliminar
  (lambda (terms expo)
    (cases terminos terms
      (sin-terminos ()
        (eopl:error
         'buscar-eliminar
         "Lo siento: el polinomio no tiene termino con ese exponente ingresado por lo tanto no podemos eliminar termino"))

      (mas-terminos (term resto)
        (cases termino-tad term
          (termino (coef expt)
            (cases exponente expt
              (expo-nat (k)
                (cond
                  [(= k expo)
                   resto]

                  [(> expo k)
                   (eopl:error
                    'buscar-eliminar
                    "Lo siento: el polinomio no tiene termino con ese exponente ingresado por lo tanto no podemos eliminar termino")]

                  [else
                   (mas-terminos
                    term
                    (buscar-eliminar resto expo))])))))))))


;;Nombre de la funcion: buscar-insertar
;;Contrato(input and output): le entra terminos del polinomio, un coeficiente y un exponente
;;y devuelve los terminos incluyendo el termino que tiene el nuevo coeficiente y exponente .O devuelve los
;;los terminos con uno de ellos modificado por la operacion ,pero eso es en caso de que el nuevo termino tenga el mismo exponente de un termino que ya estaba en el polinomio
;;Proposito: Poder buscar el lugar exacto en donde insertar el nuevo termino o operar un termino que tiene el mismo exponente para finalmente
;;devolver los terminos con el nuevo termino ingresado.
(define buscar-insertar
  (lambda(terms coef-new expo)
    (cases terminos terms
      (sin-terminos ()
       (cond
         [(= coef-new 0) (sin-terminos)]
          [else (mas-terminos(termino
            (coef-concreto->abstracto coef-new)
                    (expo-nat expo))
           (sin-terminos))]
          ))

        (mas-terminos (term resto)
         (cases termino-tad term
          (termino (coef expt)
           (cases exponente expt
            (expo-nat (k)
             (cond
               [(= k expo)
                 (let (
                  [sum (+ coef-new
                (coef-abstracto->concreto coef))]
                  )

                (if (= sum 0)
                    ;;Si se cumple la condicion
                    resto
                    ;;Si no se cumple la condicion
                 (mas-terminos
                  (termino (coef-concreto->abstracto sum) expt)
                     resto)
                   ) )]
               [(> expo k)
                (mas-terminos
                 (termino (coef-concreto->abstracto coef-new)
                  (expo-nat expo) ) terms)]
               [else (mas-terminos term
                  (buscar-insertar resto coef-new expo))]
              ))
             )))
         ))
    ))


;; Nombre de la función: variable->simbolo
;; Contrato: variable -> símbolo.
;; Propósito: extraer el símbolo de una variable abstracta. Se usa en sumar
;; para comparar las variables de los dos polinomios.
(define variable->simbolo
  (lambda (v)
    (cases variable v
      (nombre-var (s) s))))


;; Nombre de la función: termino->k
;; Contrato: termino-tad -> entero no negativo.
;; Propósito: obtener el exponente concreto de un término.
(define termino->k
  (lambda (t)
    (cases termino-tad t
      (termino (coef expo)
        (cases exponente expo
          (expo-nat (k) k))))))


;; Nombre de la función: termino->numero
;; Contrato: termino-tad -> número exacto.
;; Propósito: obtener el coeficiente concreto de un término.
(define termino->numero
  (lambda (t)
    (cases termino-tad t
      (termino (coef expo)
        (coef-abstracto->concreto coef)))))


;; Nombre de la función: sumar-terminos
;; Contrato: términos x términos -> términos.
;; Propósito: mezclar en paralelo, en una sola pasada, dos listas de términos
;; ordenadas de forma descendente. Si un exponente aparece en una sola lista,
;; el término pasa igual. Si aparece en ambas, se suman los coeficientes y,
;; si la suma es cero, el término desaparece.
(define sumar-terminos
  (lambda (terms-p terms-q)
    (cases terminos terms-p
      (sin-terminos () terms-q)
      (mas-terminos (term-p resto-p)
        (cases terminos terms-q
          (sin-terminos () terms-p)
          (mas-terminos (term-q resto-q)
            (let ([kp (termino->k term-p)]
                  [kq (termino->k term-q)])
              (cond
                ;; El exponente de p es mayor: el término de p pasa igual.
                [(> kp kq)
                 (mas-terminos term-p (sumar-terminos resto-p terms-q))]
                ;; El exponente de q es mayor: el término de q pasa igual.
                [(< kp kq)
                 (mas-terminos term-q (sumar-terminos terms-p resto-q))]
                ;; Exponentes iguales: se suman los coeficientes.
                [else
                 (let ([suma (+ (termino->numero term-p)
                                (termino->numero term-q))])
                   (if (= suma 0)
                       (sumar-terminos resto-p resto-q)
                       (mas-terminos
                        (termino (coef-concreto->abstracto suma)
                                 (expo-nat kp))
                        (sumar-terminos resto-p resto-q))))]))))))))


;;Area del Programador


;;Nombre de la funcion: polinomio-cero
;;Contrato(input and output): le entra un simbolo ejemplo 'x
;;y devuelve un polinomio abstracto con su nombre de variable indicada y sin termino en la parte de terminos
;;Proposito: Poder construir un polinomio sin terminos en la variable dada a construir
(define polinomio-cero
  (lambda (variable)
    (cond
      [(symbol? variable)
       (poli
        (nombre-var variable)
        (sin-terminos))]
      [else
       (eopl:error
        'polinomio-cero
        "La variable debe ser un simbolo")])))


;;Nombre de la funcion: insertar-termino
;;Contrato(input and output): le entra un polinomio, un coeficiente y un exponente
;;y devuelve un polinomio con un nuevo termino insertado o en caso de que haya
;;un termino con el mismo exponente lo opera y devuelve el polinomio ya con ese termino operado
;;Proposito: Poder insertar terminos a un polinomio que trae terminos.Tambien armar un polinomio desde 0 usando esta funcion
;;finalmente otro objetivo es el de operar un termino ya existente para modificar su valor solo si tienen el mismo exponente
(define insertar-termino
  (lambda (p coeficiente exponente)
    (cond
       [(not(and (number? coeficiente)
             (exact? coeficiente) )) (eopl:error 'insertar-termino
       "Lo siento: el coeficiente debe ser un numero exacto")]
       [(not(and (integer? exponente) (exact? exponente) (>= exponente 0)
             )) (eopl:error 'insertar-termino
       "Lo siento: el exponente debe ser un entero no negativo")]
       [(= coeficiente 0) p]
       [else (cases polinomio p
        (poli(var terms)
          (poli var
           (buscar-insertar
            terms coeficiente exponente))))
        ]
       )))


;;Nombre de la funcion: coeficiente-de
;;Contrato(input and output): le entra un polinomio y un exponente
;;y devuelve un coeficiente del polinomio asociado al exponente establecido
;;Proposito: Poder obtener adecuadamente el coeficiente que esta asociado ala potencia ingresada anterior mente
;;ejecutandose la logica pesada en la funcion buscar-coeficiente
(define coeficiente-de
  (lambda (p exponente)
    (if (not (and (integer? exponente)
                  (exact? exponente)
                  (>= exponente 0)))
        (eopl:error
         'coeficiente-de
         "Lo siento: el exponente debe ser un entero no negativo")
        (cases polinomio p
          (poli (var terms)
            (buscar-coeficiente terms exponente))))))


;;Nombre de la funcion: eliminar-termino
;;Contrato(input and output): le entra un polinomio y el exponente del termino a borrar
;;y devuelve un nuevo polinomio abstracto sin ese termino
;;Proposito: Poder eliminar un termino del polinomio por medio de un exponente que indica que termino borrar junto con el exponente fue establecido
(define eliminar-termino
  (lambda (p exponente)
    (if (not (and (integer? exponente)
                  (exact? exponente)
                  (>= exponente 0)))
        (eopl:error
         'eliminar-termino
         "Lo siento: el exponente debe ser un entero no negativo")
        (cases polinomio p
          (poli (var terms)
            (poli
             var
             (buscar-eliminar terms exponente)))))))


;; Nombre de la función: sumar
;; Contrato: polinomio x polinomio -> polinomio.
;; Propósito: sumar dos polinomios que tengan la misma variable.
;; Recorre los términos de ambos en paralelo, una sola vez, con
;; sumar-terminos. Si hay exponentes iguales, los coeficientes se suman.
;; Si un coeficiente resultante es cero, el término se elimina.
;; Genera un error si las variables son distintas.
(define sumar
  (lambda (p q)
    (cases polinomio p
      (poli (var-p terms-p)
        (cases polinomio q
          (poli (var-q terms-q)
            (if (eqv? (variable->simbolo var-p)
                      (variable->simbolo var-q))
                (poli var-p (sumar-terminos terms-p terms-q))
                (eopl:error
                 'sumar
                 "Los polinomios deben tener la misma variable"))))))))


;; Ejemplo de polinomio (para el informe; no se define aquí porque los
;; archivos de implementación no llevan datos de prueba):
;; p = 4x^5 - (3/2)x^2 + 7
;; (poli (nombre-var 'x)
;;       (mas-terminos (termino (coef-ent 4) (expo-nat 5))
;;         (mas-terminos (termino (coef-rac -3 2) (expo-nat 2))
;;           (mas-terminos (termino (coef-ent 7) (expo-nat 0))
;;             (sin-terminos)))))


;;=====================================================
;;Ejemplos de construccion y uso (representacion con datatypes)
;;No se definen como codigo porque los archivos de implementacion
;;no llevan pruebas. Cada ejemplo muestra la expresion y lo que devuelve.
;;=====================================================

;;Polinomio que se usa en varios ejemplos:
;;p = 4x^5 - (3/2)x^2 + 7
;;p = (insertar-termino
;;      (insertar-termino
;;        (insertar-termino (polinomio-cero 'x) 4 5)
;;        -3/2 2)
;;      7 0)


;;Construccion de los datos (datatypes)
;;1. (coef-ent 5)                       => el coeficiente entero 5
;;2. (coef-rac 3 2)                     => el coeficiente racional 3/2
;;3. (expo-nat 4)                       => el exponente 4
;;4. (termino (coef-ent 4) (expo-nat 5))=> el termino 4x^5
;;5. (poli (nombre-var 'x) (sin-terminos))
;;                                      => el polinomio cero en x


;;polinomio-cero
;;1. (polinomio-cero 'x)  => polinomio sin terminos en la variable x
;;2. (polinomio-cero 'y)  => polinomio sin terminos en la variable y
;;3. (polinomio-cero 'z)  => polinomio sin terminos en la variable z
;;4. (coeficiente-de (polinomio-cero 'x) 0)
;;                        => error, el polinomio cero no tiene terminos
;;5. (polinomio-cero 5)   => error, la variable debe ser un simbolo


;;insertar-termino
;;1. (insertar-termino (polinomio-cero 'x) 7 0)
;;   => el polinomio 7
;;2. (insertar-termino (polinomio-cero 'x) 3/2 1)
;;   => el polinomio (3/2)x
;;3. (insertar-termino (insertar-termino (polinomio-cero 'x) 7 0) 4 5)
;;   => 4x^5 + 7 (el termino de mayor exponente queda primero)
;;4. (insertar-termino (insertar-termino (polinomio-cero 'x) 3 2) 5 2)
;;   => 8x^2 (mismo exponente: se suman los coeficientes)
;;5. (insertar-termino (insertar-termino (polinomio-cero 'x) 3 2) -3 2)
;;   => el polinomio cero (la suma da 0 y el termino desaparece)


;;coeficiente-de   (con p = 4x^5 - (3/2)x^2 + 7)
;;1. (coeficiente-de p 5)   => 4
;;2. (coeficiente-de p 2)   => -3/2
;;3. (coeficiente-de p 0)   => 7
;;4. (coeficiente-de p 3)   => error, p no tiene termino con exponente 3
;;5. (coeficiente-de p -1)  => error, el exponente debe ser un entero no negativo


;;eliminar-termino   (con p = 4x^5 - (3/2)x^2 + 7)
;;1. (eliminar-termino p 5)
;;   => -(3/2)x^2 + 7, y (coeficiente-de ... 2) da -3/2
;;2. (eliminar-termino p 2)
;;   => 4x^5 + 7, y (coeficiente-de ... 0) da 7
;;3. (eliminar-termino p 0)
;;   => 4x^5 - (3/2)x^2, y (coeficiente-de ... 5) da 4
;;4. (eliminar-termino (eliminar-termino (eliminar-termino p 5) 2) 0)
;;   => el polinomio cero
;;5. (eliminar-termino p 3)
;;   => error, p no tiene termino con exponente 3


;;sumar
;;1. (sumar p (polinomio-cero 'x))
;;   => p sin cambios

;;2.
;;(sumar (4x^5 + 3x^2) (2x^5 + x^2))
;;
;;(sumar (insertar-termino
;;(insertar-termino (polinomio-cero 'x) 3 2) 
;;    4 5)  (insertar-termino
;;  (insertar-termino (polinomio-cero 'x) 1 2)
;;      2 5)) 
;;   => 6x^5 + 4x^2


;;3.
;;(sumar (x^3) (x))
;; (sumar (insertar-termino (polinomio-cero 'x)
;;      1 3) (insertar-termino (polinomio-cero 'x)
;;          1 1))
;;   => x^3 + x (los exponentes distintos se mezclan en orden)


;;4.
;;(sumar(3x^2) (-3x^2))
;;(sumar (insertar-termino (polinomio-cero 'x)
;;      3 2) (insertar-termino (polinomio-cero 'x)
;;   -3 2))
;;   => el polinomio cero (los coeficientes se cancelan)

;;5. (sumar (polinomio-cero 'x) (polinomio-cero 'y))
;;   => error, los polinomios deben tener la misma variable