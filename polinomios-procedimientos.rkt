#lang eopl
;Autores: Andres Felipe Quiceno gil Codigo:2477362
;Yonier Alejandro Vega Rojas Codigo:2477056
;Jhon Fabricio Hurtado Marin Codigo: 2459472
;Juan Esteban Aguirre Castañeda Codigo:2459676

;; Taller 1 — Polinomios dispersos.
;; Parte 2: representación basada en procedimientos.
;;
;; Interfaz del TAD. Cada función va comentada con su nombre, su contrato
;; (entrada -> salida) y su propósito, y ninguna recorre la lista de términos
;; más de una vez ni la ordena al final.
;;
;;   polinomio-cero    : symbol -> polinomio
;;   insertar-termino  : polinomio x coeficiente x exponente -> polinomio
;;   coeficiente-de    : polinomio x exponente -> coeficiente
;;   eliminar-termino  : polinomio x exponente -> polinomio


;; Se exportan los constructores, predicados y extractores de la
;; representación basada en procedimientos para poder utilizarlos en los
;; ejemplos y en el archivo de pruebas. También se exportan las cuatro
;; funciones principales de la interfaz del TAD.
(provide polinomio-cero insertar-termino coeficiente-de eliminar-termino
         poli nombre-var sin-terminos mas-terminos termino coef-ent coef-rac expo-nat
         poli? nombre-var? sin-terminos? mas-terminos? termino? coef-ent? coef-rac? expo-nat?
         poli->var poli->terms nombre-var->s mas-terminos->term mas-terminos->resto
         termino->coef termino->expo coef-ent->n coef-rac->num coef-rac->den expo-nat->k)


;;Interfaz:
;;Constructores
;;En esta representacion cada dato es un procedimiento que recibe un
;;selector (un numero) y responde segun el mensaje:
;;0 devuelve la etiqueta de la variante y 1, 2 devuelven sus campos.

;;Nombre de la funcion: poli
;;Contrato(input and output): recibe una variable y una lista de terminos
;;y devuelve un polinomio representado mediante un procedimiento.
;;Proposito: construir la representacion de un polinomio.
(define poli ;<polinomio>
  (lambda (var terms)
    (lambda (s)
      (cond
        [(= s 0) 'poli]
        [(= s 1) var]
        [(= s 2) terms]
        [else (eopl:error 'poli "selector invalido")]))))


;;Nombre de la funcion: nombre-var
;;Contrato(input and output): recibe un simbolo y devuelve una variable
;;representada mediante un procedimiento.
;;Proposito: construir la representacion de una variable.
(define nombre-var ;<variable>
  (lambda (sym)
    (lambda (s)
      (cond
        [(= s 0) 'nombre-var]
        [(= s 1) sym]
        [else (eopl:error 'nombre-var "selector invalido")]))))


;;Nombre de la funcion: sin-terminos
;;Contrato(input and output): no recibe argumentos y devuelve un
;;procedimiento que representa una lista de terminos vacia.
;;Proposito: construir la representacion de un polinomio sin terminos.
(define sin-terminos ;<terminos> sin terminos
  (lambda ()
    (lambda (s)
      (cond
        [(= s 0) 'sin-terminos]
        [else (eopl:error 'sin-terminos "selector invalido")]))))


;;Nombre de la funcion: mas-terminos
;;Contrato(input and output): recibe un termino y el resto de una lista
;;de terminos y devuelve la representacion de una lista de terminos.
;;Proposito: construir una lista de terminos que contiene un termino
;;y el resto de los terminos.
(define mas-terminos  ;<terminos> con terminos
  (lambda (term resto)
    (lambda (s)
      (cond
        [(= s 0) 'mas-terminos]
        [(= s 1) term]
        [(= s 2) resto]
        [else (eopl:error 'mas-terminos "selector invalido")]))))


;;Nombre de la funcion: termino
;;Contrato(input and output): recibe un coeficiente y un exponente
;;y devuelve un termino representado mediante un procedimiento.
;;Proposito: construir la representacion de un termino.
(define termino ;<termino>
  (lambda (coef expo)
    (lambda (s)
      (cond
        [(= s 0) 'termino]
        [(= s 1) coef]
        [(= s 2) expo]
        [else (eopl:error 'termino "selector invalido")]))))


;;Nombre de la funcion: coef-ent
;;Contrato(input and output): recibe un numero entero y devuelve un
;;coeficiente entero representado mediante un procedimiento.
;;Proposito: construir la representacion de un coeficiente entero.
(define coef-ent ;<coeficiente> coeficiente entero
  (lambda (n)
    (lambda (s)
      (cond
        [(= s 0) 'coef-ent]
        [(= s 1) n]
        [else (eopl:error 'coef-ent "selector invalido")]))))


;;Nombre de la funcion: coef-rac
;;Contrato(input and output): recibe numerador y denominador y devuelve
;;un coeficiente racional representado mediante un procedimiento.
;;Proposito: construir la representacion de un coeficiente racional.
(define coef-rac ;<coeficiente> coeficiente racional
  (lambda (num den)
    (lambda (s)
      (cond
        [(= s 0) 'coef-rac]
        [(= s 1) num]
        [(= s 2) den]
        [else (eopl:error 'coef-rac "selector invalido")]))))


;;Nombre de la funcion: expo-nat
;;Contrato(input and output): recibe un entero natural y devuelve un
;;exponente representado mediante un procedimiento.
;;Proposito: construir la representacion de un exponente natural.
(define expo-nat ;<exponente>
  (lambda (k)
    (lambda (s)
      (cond
        [(= s 0) 'expo-nat]
        [(= s 1) k]
        [else (eopl:error 'expo-nat "selector invalido")]))))


;;Predicados
;;validar si en efecto la expresion pertenece a ese tipo de dato

;;Nombre de la funcion: tipo?
;;Contrato(input and output): recibe una etiqueta (simbolo) y devuelve
;;un predicado que recibe una expresion y devuelve #t o #f.
;;Proposito: fabricar los predicados de cada variante. Se usa procedure?
;;para que no de error si la expresion dada no es un procedimiento, y luego
;;se le pide la etiqueta con el selector 0.
(define tipo?
  (lambda (etiqueta)
    (lambda (x)
      (and (procedure? x) (eq? (x 0) etiqueta)))))

;;Nombre de la funcion: poli? nombre-var? sin-terminos? mas-terminos? termino?
;;coef-ent? coef-rac? expo-nat?
;;Contrato(input and output): recibe una expresion y devuelve #t si
;;corresponde a esa variante y #f en caso contrario.
;;Proposito: identificar cada variante de la gramatica.
(define poli?          (tipo? 'poli))
(define nombre-var?    (tipo? 'nombre-var))
(define sin-terminos?  (tipo? 'sin-terminos))
(define mas-terminos?  (tipo? 'mas-terminos))
(define termino?       (tipo? 'termino))
(define coef-ent?      (tipo? 'coef-ent))
(define coef-rac?      (tipo? 'coef-rac))
(define expo-nat?      (tipo? 'expo-nat))


;;Extractores
;;Contrato(input and output) de todos: recibe un dato de la variante
;;correspondiente y devuelve el valor del campo indicado en el nombre.
;;Proposito: consultar los campos de cada dato usando el selector 1 o 2.

(define poli->var            (lambda (p) (p 1)))  ;<polinomio> variable
(define poli->terms          (lambda (p) (p 2)))  ;<polinomio> terminos
(define nombre-var->s        (lambda (v) (v 1)))  ;<variable> simbolo
(define mas-terminos->term   (lambda (t) (t 1)))  ;<terminos> primer termino
(define mas-terminos->resto  (lambda (t) (t 2)))  ;<terminos> resto
(define termino->coef        (lambda (t) (t 1)))  ;<termino> coeficiente
(define termino->expo        (lambda (t) (t 2)))  ;<termino> exponente
(define coef-ent->n          (lambda (c) (c 1)))  ;<coeficiente> entero n
(define coef-rac->num        (lambda (c) (c 1)))  ;<coeficiente> numerador
(define coef-rac->den        (lambda (c) (c 2)))  ;<coeficiente> denominador
(define expo-nat->k          (lambda (e) (e 1)))  ;<exponente> valor k


;;Funciones Auxiliares
;;(A partir de aqui el codigo es identico al de polinomios-listas.rkt,
;;porque solo usa constructores y observadores, nunca la estructura interna)


;;Nombre de la funcion: coef-abstracto->concreto
;;Contrato(input and output): le entra un coeficiente abstracto(termino->coef term)
;;y devuelve el coeficiente concreto es decir un coeficiente entero o racional
;;(numerador/denominador) en este caso
;;Proposito: poder convertir un termino abstracto a un termino concreto con el objetivo de mostrar la solucion en forma concreta
;;y tambien se creo la funcion con el proposito de segmentar mejor el codigo y no dejar todo el codigo en una sola funcion.
(define coef-abstracto->concreto
  (lambda (exp)
    (cond
      [(coef-ent? exp) (coef-ent->n exp)]
      [(coef-rac? exp) (/ (coef-rac->num exp)
                          (coef-rac->den exp))]
      [else (eopl:error
             'coef-abstracto->concreto
             "coeficiente invalido")]
      )
    ))


;;Nombre de la funcion: coef-concreto->abstracto
;;Contrato(input and output): le entra un coeficiente concreto ejemplo un 5 y debe devolver un coeficiente abstracto
;;respetando su tipo de representacion si es entero o racional. ejemplo de salida (coef-ent 5)
;;Proposito: poder convertir un coeficiente concreto a un coeficiente abstracto con su respectiva representacion
(define coef-concreto->abstracto
  (lambda (exp)
    (cond ;numerator denominator
      [(integer? exp) (coef-ent exp)]
      [(and (rational? exp) (exact? exp))
       (coef-rac (numerator exp)
                 (denominator exp))]
      [else (eopl:error
             'coef-concreto->abstracto
             "coeficiente invalido")]
      )
    ))


;;Nombre de la funcion: buscar-coeficiente
;;Contrato(input and output): le entra los terminos del polinomio establecido y el exponente establecido por el usuario
;;y devuelve el coeficiente asociado al exponente establecido por el usuario.En
;;caso de que no exista el exponente en el polinomio se devuelve un error respecto a que ese exponente no esta asociado a ningun coeficiente
;;Proposito: poder buscar el coeficiente asociado al exponente ingresado. Su otro proposito es que se creo para trabajar directamente con los terminos
;;y exponente del polinomio ya de forma organizada y separada .Dejando la funcion principal coeficiente-de mas limpia
(define buscar-coeficiente
  (lambda(terms expo)
    (cond
      [(sin-terminos? terms)
       (eopl:error
        'buscar-coeficiente
        "Lo siento: el polinomio no tiene termino con ese exponente ingresado")]
      [else
       (let* (   ;;Funcion auxiliar dentro de funcion buscar
              [term (mas-terminos->term terms)]  ;Define variables que se van a ligar en la zona exterior
              [resto (mas-terminos->resto terms)]
              [expt (termino->expo term)]
              )
          (cond
            [(= (expo-nat->k expt) expo)
             (coef-abstracto->concreto
              (termino->coef term))]
            [(> expo (expo-nat->k expt))
             (eopl:error
              'buscar-coeficiente
              "Lo siento: el polinomio no tiene termino con ese exponente ingresado")]
            [else (buscar-coeficiente resto expo)]
            )
         )]
      )
    ))


;;Nombre de la funcion: buscar-eliminar
;;Contrato(input and output): le entra los terminos del polinomio y el exponente del termino a eliminar
;;y devuelve un polinomio nuevo sin ese termino borrado
;;Proposito: Poder buscar donde esta el termino que tiene asociado el exponente a borrar y posteriormente borrarlo para
;;devolver los terminos del polinomio sin ese termino que se solicito borrar por medio del exponente
(define buscar-eliminar
  (lambda(terms expo)
    (cond
      [(sin-terminos? terms)
       (eopl:error
        'buscar-eliminar
        "Lo siento: el polinomio no tiene termino con ese exponente ingresado por lo tanto no podemos eliminar termino")]
      [else
       (let* (   ;;Funcion auxiliar dentro de funcion buscar
              [term (mas-terminos->term terms)]  ;Define variables que se van a ligar en la zona exterior
              [resto (mas-terminos->resto terms)]
              [expt (termino->expo term)]
              )
          (cond
            [(= (expo-nat->k expt) expo)
             resto]
            [(> expo (expo-nat->k expt))
             (eopl:error
              'buscar-eliminar
              "Lo siento: el polinomio no tiene termino con ese exponente ingresado por lo tanto no podemos eliminar termino")]
            [else
             (mas-terminos term (buscar-eliminar resto expo))]
            )
         )]
      )
    ))


;;Nombre de la funcion: buscar-insertar
;;Contrato(input and output): le entra terminos del polinomio, un coeficiente y un exponente
;;y devuelve los terminos incluyendo el termino que tiene el nuevo coeficiente y exponente .O devuelve los
;;los terminos con uno de ellos modificado por la operacion ,pero eso es en caso de que el nuevo termino tenga el mismo exponente de un termino que ya estaba en el polinomio
;;Proposito: Poder buscar el lugar exacto en donde insertar el nuevo termino o operar un termino que tiene el mismo exponente para finalmente
;;devolver los terminos con el nuevo termino ingresado.
(define buscar-insertar
  (lambda(terms coef-new expo)
    (cond
      [(sin-terminos? terms)
       (cond  ;Es un verificador en caso que se use de golpe la funcion buscar-insertar
         [(= coef-new 0)
          (sin-terminos)]
         [else
          (mas-terminos
           (termino
            (coef-concreto->abstracto coef-new)
            (expo-nat expo))
           (sin-terminos))]
         )
       ]
      [else
       (let* (   ;;Funcion auxiliar dentro de funcion buscar
              [term (mas-terminos->term terms)]  ;Define variables que se van a ligar en la zona exterior
              [resto (mas-terminos->resto terms)]
              [expt (termino->expo term)]
              )
          (cond
            [(= (expo-nat->k expt) expo)
             (if (= (+ coef-new
                       (coef-abstracto->concreto
                        (termino->coef term))) 0)
                 ;;Si se cumple la condicion
                 resto
                 ;;Sino se cumple la condicion
                 (mas-terminos
                  (termino
                   (coef-concreto->abstracto
                    (+ coef-new
                       (coef-abstracto->concreto
                        (termino->coef term))))
                   (expo-nat expo))
                  resto)
                 )
             ]
            [(> expo (expo-nat->k expt))
             (mas-terminos
              (termino
               (coef-concreto->abstracto coef-new)
               (expo-nat expo))
              terms)]
            [else
             (mas-terminos term
                           (buscar-insertar resto coef-new expo))]
            )
         )]
      )
    ))


;;Area del Programador


;;Nombre de la funcion: polinomio-cero
;;Contrato(input and output): le entra un simbolo ejemplo 'x
;;y devuelve un polinomio abstracto con su nombre de variable indicada y sin termino en la parte de terminos
;;Proposito: Poder construir un polinomio sin terminos en la variable dada a construir
(define polinomio-cero
  (lambda (variable)
    (cond
      [(symbol? variable);Validar que en efecto la variable sea un simbolo
       (poli (nombre-var variable) (sin-terminos))]
      [else
       (eopl:error
        'polinomio-cero
        "la variable debe ser un simbolo")]
      )))


;;Nombre de la funcion: insertar-termino
;;Contrato(input and output): le entra un polinomio, un coeficiente y un exponente
;;y devuelve un polinomio con un nuevo termino insertado o en caso de que haya
;;un termino con el mismo exponente lo opera y devuelve el polinomio ya con ese termino operado
;;Proposito: Poder insertar terminos a un polinomio que trae terminos.Tambien armar un polinomio desde 0 usando esta funcion
;;finalmente otro objetivo es el de operar un termino ya existente para modificar su valor solo si tienen el mismo exponente
(define insertar-termino
  (lambda (polinomio coeficiente exponente)
    (cond
      [(not (and (number? coeficiente)
                 (exact? coeficiente)))
       (eopl:error
        'insertar-termino
        "Lo siento: el coeficiente debe ser un numero exacto")]
      [(not (and (integer? exponente)
                 (exact? exponente)
                 (>= exponente 0)))
       (eopl:error
        'insertar-termino
        "Lo siento: el exponente debe ser un entero no negativo")]
      [(= coeficiente 0)
       polinomio]
      [else
       (poli
        (poli->var polinomio)
        (buscar-insertar
         (poli->terms polinomio)
         coeficiente
         exponente))
       ]
      )
    ))


;;Nombre de la funcion: coeficiente-de
;;Contrato(input and output): le entra un polinomio y un exponente
;;y devuelve un coeficiente del polinomio asociado al exponente establecido
;;Proposito: Poder obtener adecuadamente el coeficiente que esta asociado ala potencia ingresada anterior mente
;;ejecutandose la logica pesada en la funcion buscar-coeficiente
(define coeficiente-de
  (lambda (polinomio exponente)
    (cond
      [(not (and (integer? exponente)
                 (exact? exponente)
                 (>= exponente 0)))
       (eopl:error
        'coeficiente-de
        "Lo siento: el exponente debe ser un entero no negativo")]
      [else
       (buscar-coeficiente
        (poli->terms polinomio)
        exponente)]
      )
    ))


;;Nombre de la funcion: eliminar-termino
;;Contrato(input and output): le entra un polinomio y el exponente del termino a borrar
;;y devuelve un nuevo polinomio abstracto sin ese termino
;;Proposito: Poder eliminar un termino del polinomio por medio de un exponente que indica que termino borrar junto con el exponente fue establecido
(define eliminar-termino
  (lambda (polinomio exponente)
    (cond
      [(not (and (integer? exponente)
                 (exact? exponente)
                 (>= exponente 0)))
       (eopl:error
        'eliminar-termino
        "Lo siento: el exponente debe ser un entero no negativo")]
      [else
       (poli
        (poli->var polinomio)
        (buscar-eliminar
         (poli->terms polinomio)
         exponente))
       ]
      )
    ))