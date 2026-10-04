#lang eopl

;;Autores: Andres Felipe Quiceno Gil Codigo: 2477362
;;Yonier Alejandro Vega Rojas Codigo: 2477056
;;Jhon Fabricio Hurtado Marin Codigo: 2459472
;;Juan Esteban Aguirre Castañeda Codigo: 2459676

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


;; =========================
;; Funciones auxiliares
;; =========================

;; Nombre de la función: coef-abstracto->concreto
;; Contrato: coeficiente abstracto -> número concreto.
;; Propósito: convertir la representación abstracta del coeficiente
;; en un número que se pueda utilizar en las operaciones aritméticas.
;; Si es entero devuelve el entero; si es racional devuelve la fracción.
(define coef-abstracto->concreto
  (lambda (exp)
    (cases coeficiente exp
      (coef-ent (n)
        n)
      (coef-rac (num den)
        (/ num den)))))


;; Nombre de la función: coef-concreto->abstracto
;; Contrato: número racional exacto -> coeficiente abstracto.
;; Propósito: convertir un coeficiente concreto en su representación
;; abstracta, respetando si es entero o racional.
;; Ejemplo: 5 se convierte en (coef-ent 5).
;; Ejemplo: 3/2 se convierte en (coef-rac 3 2).
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


;; Nombre de la función: buscar-coeficiente
;; Contrato: términos x exponente -> coeficiente concreto.
;; Propósito: buscar el coeficiente asociado al exponente ingresado.
;; Si el exponente no existe, genera un error.
;; Como los términos están ordenados de mayor a menor exponente,
;; si el exponente buscado es mayor que el actual, no puede existir
;; más adelante en la lista.
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


;; Nombre de la función: buscar-eliminar
;; Contrato: términos x exponente -> términos.
;; Propósito: buscar el término asociado al exponente indicado y
;; devolver la lista de términos sin ese término.
;; Si el exponente no existe, genera un error.
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


;; Nombre de la función: buscar-insertar
;; Contrato: términos x coeficiente x exponente -> términos.
;; Propósito: encontrar la posición correcta para insertar un término.
;; Si ya existe un término con el mismo exponente, suma los coeficientes.
;; Si la suma es cero, elimina el término.
;; La lista se mantiene ordenada por exponentes descendentes.
(define buscar-insertar
  (lambda (terms coef-new expo)
    (cases terminos terms
      (sin-terminos ()
        (if (= coef-new 0)
            (sin-terminos)
            (mas-terminos
             (termino
              (coef-concreto->abstracto coef-new)
              (expo-nat expo))
             (sin-terminos))))
      (mas-terminos (term resto)
        (cases termino-tad term
          (termino (coef expt)
            (cases exponente expt
              (expo-nat (k)
                (cond
                  ;; Mismo exponente: se suman los coeficientes.
                  [(= k expo)
                   (let ([suma (+ coef-new
                                  (coef-abstracto->concreto coef))])
                     (if (= suma 0)
                         resto
                         (mas-terminos
                          (termino
                           (coef-concreto->abstracto suma)
                           expt)
                          resto)))]
                  ;; El nuevo exponente es mayor: va antes del término actual.
                  [(> expo k)
                   (mas-terminos
                    (termino
                     (coef-concreto->abstracto coef-new)
                     (expo-nat expo))
                    terms)]
                  ;; El nuevo exponente es menor: se sigue buscando en el resto.
                  [else
                   (mas-terminos
                    term
                    (buscar-insertar resto coef-new expo))])))))))))


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


;; =========================
;; Área del programador
;; =========================

;; Nombre de la función: polinomio-cero
;; Contrato: símbolo -> polinomio.
;; Propósito: construir un polinomio sin términos para la variable dada.
;; Ejemplo: (polinomio-cero 'x) crea el polinomio cero en x.
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


;; Nombre de la función: insertar-termino
;; Contrato: polinomio x coeficiente x exponente -> polinomio.
;; Propósito: insertar un término en el polinomio.
;; Si ya existe un término con el mismo exponente, suma los coeficientes.
;; Si la suma da cero, el término desaparece.
;; Los coeficientes deben ser números racionales exactos y los exponentes
;; deben ser enteros no negativos.
;; Un coeficiente 0 deja el polinomio sin cambios.
(define insertar-termino
  (lambda (p coeficiente exponente)
    (cond
      [(not (and (number? coeficiente)
                 (rational? coeficiente)
                 (exact? coeficiente)))
       (eopl:error
        'insertar-termino
        "El coeficiente debe ser un numero racional exacto")]
      [(not (and (integer? exponente)
                 (exact? exponente)
                 (>= exponente 0)))
       (eopl:error
        'insertar-termino
        "El exponente debe ser un entero no negativo")]
      ;; Con coeficiente cero no se inserta nada, para no romper el invariante.
      [(= coeficiente 0) p]
      [else
       (cases polinomio p
         (poli (var terms)
           (poli
            var
            (buscar-insertar
             terms
             coeficiente
             exponente))))])))


;; Nombre de la función: coeficiente-de
;; Contrato: polinomio x exponente -> coeficiente concreto.
;; Propósito: obtener el coeficiente asociado al exponente indicado.
;; La búsqueda se realiza mediante buscar-coeficiente.
(define coeficiente-de
  (lambda (p exponente)
    (if (not (and (integer? exponente)
                  (exact? exponente)
                  (>= exponente 0)))
        (eopl:error
         'coeficiente-de
         "El exponente debe ser un entero no negativo")
        (cases polinomio p
          (poli (var terms)
            (buscar-coeficiente terms exponente))))))


;; Nombre de la función: eliminar-termino
;; Contrato: polinomio x exponente -> polinomio.
;; Propósito: devolver un polinomio nuevo sin el término que tiene
;; el exponente indicado.
;; La variable original se conserva.
(define eliminar-termino
  (lambda (p exponente)
    (if (not (and (integer? exponente)
                  (exact? exponente)
                  (>= exponente 0)))
        (eopl:error
         'eliminar-termino
         "El exponente debe ser un entero no negativo")
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