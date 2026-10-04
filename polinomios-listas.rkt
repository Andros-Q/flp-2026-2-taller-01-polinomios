#lang eopl
;Autores: Andres Felipe Quiceno gil Codigo:2477362
;Yonier Alejandro Vega Rojas Codigo:2477056
;Jhon Fabricio Hurtado Marin Codigo: 2459472
;Juan Esteban Aguirre Castañeda Codigo:2459676

;;Taller 1 - Polinomios dispersos
;;Parte 1: representacion basada en listas

;;Las funciones del TAD son:
;;polinomio-cero, insertar-termino, coeficiente-de y eliminar-termino


;;Se exportan las funciones del TAD, los constructores, los predicados y los
;;extractores para poder usarlos en el archivo de pruebas
(provide
 poli
 nombre-var
 sin-terminos
 mas-terminos
 termino
 coef-ent
 coef-rac
 expo-nat

 poli?
 nombre-var?
 sin-terminos?
 mas-terminos?
 termino?
 coef-ent?
 coef-rac?
 expo-nat?

 poli->var
 poli->terms
 nombre-var->s
 mas-terminos->term
 mas-terminos->resto
 termino->coef
 termino->expo
 coef-ent->n
 coef-rac->num
 coef-rac->den
 expo-nat->k

 polinomio-cero
 insertar-termino
 coeficiente-de
 eliminar-termino)


;;Interfaz:
;;Constructores

;;Nombre de la funcion: poli
;;Contrato(input and output): recibe una variable y una lista de terminos
;;y devuelve un polinomio representado mediante una lista.
;;Proposito: construir la representacion concreta de un polinomio.
(define poli  ;<polinomio>
  (lambda(var terms)
    (list 'poli var terms)
    ))


;;Nombre de la funcion: nombre-var
;;Contrato(input and output): recibe un simbolo y devuelve una variable
;;representada mediante una lista.
;;Proposito: construir la representacion abstracta de una variable.
(define nombre-var ;<variable>
  (lambda(s)
    (list 'nombre-var s)
    ))


;;Nombre de la funcion: sin-terminos
;;Contrato(input and output): no recibe argumentos y devuelve una lista
;;que representa una lista de terminos vacia.
;;Proposito: construir la representacion de un polinomio sin terminos.
(define sin-terminos ;<terminos> sin terminos
  (lambda()
    (list 'sin-terminos)
    ))


;;Nombre de la funcion: mas-terminos
;;Contrato(input and output): recibe un termino y el resto de una lista
;;de terminos y devuelve la representacion de una lista de terminos.
;;Proposito: construir una lista de terminos que contiene un termino
;;y el resto de los terminos.
(define mas-terminos ;<terminos> con terminos
  (lambda(term resto)
    (list 'mas-terminos term resto)
    ))


;;Nombre de la funcion: termino
;;Contrato(input and output): recibe un coeficiente y un exponente
;;y devuelve un termino representado mediante una lista.
;;Proposito: construir la representacion abstracta de un termino.
(define termino ;<termino>
  (lambda(coef expo)
    (list 'termino coef expo)
    ))


;;Nombre de la funcion: coef-ent
;;Contrato(input and output): recibe un numero entero y devuelve un
;;coeficiente entero representado mediante una lista.
;;Proposito: construir la representacion abstracta de un coeficiente entero.
(define coef-ent ;<coeficiente> coeficiente entero
  (lambda(n)
    (list 'coef-ent n)
    ))


;;Nombre de la funcion: coef-rac
;;Contrato(input and output): recibe numerador y denominador y devuelve
;;un coeficiente racional representado mediante una lista.
;;Proposito: construir la representacion abstracta de un coeficiente racional.
(define coef-rac ;<coeficiente> coeficiente racional
  (lambda(num den)
    (list 'coef-rac num den)
    ))


;;Nombre de la funcion: expo-nat
;;Contrato(input and output): recibe un entero natural y devuelve un
;;exponente representado mediante una lista.
;;Proposito: construir la representacion abstracta de un exponente natural.
(define expo-nat ;<exponente>
  (lambda(k)
    (list 'expo-nat k)
    ))


;;Predicados
;;validar si en efecto la expresion pertenece a ese tipo de dato

;;Se pone pair con el objetivo de evitar que car de error
;;si la expresion dada no llega a ser una lista

;;Nombre de la funcion: poli?
;;Contrato(input and output): recibe una expresion y devuelve #t si
;;la expresion corresponde a la representacion de un polinomio y #f
;;en caso contrario.
;;Proposito: identificar si una expresion tiene la etiqueta de polinomio.
(define poli?
  (lambda(exp)
    ;;Confirmo con pair que en efecto es una lista y luego que la expresion
    ;;en efecto coincida con el tipo de dato que quiero confirmar
    (and (pair? exp) (equal? (car exp) 'poli)
    )))


;;Nombre de la funcion: nombre-var?
;;Contrato(input and output): recibe una expresion y devuelve #t si
;;representa una variable y #f en caso contrario.
;;Proposito: identificar la representacion de una variable.
(define nombre-var?
  (lambda(exp)
   (and (pair? exp) (equal? (car exp) 'nombre-var)
    )))


;;Nombre de la funcion: sin-terminos?
;;Contrato(input and output): recibe una expresion y devuelve #t si
;;representa una lista de terminos vacia y #f en caso contrario.
;;Proposito: identificar la representacion de una lista de terminos vacia.
(define sin-terminos?
  (lambda(exp)
   (and (pair? exp) (equal? (car exp) 'sin-terminos)
    )))


;;Nombre de la funcion: mas-terminos?
;;Contrato(input and output): recibe una expresion y devuelve #t si
;;representa una lista de terminos con al menos un termino.
;;Proposito: identificar la representacion de una lista de terminos no vacia.
(define mas-terminos?
  (lambda(exp)
   (and (pair? exp) (equal? (car exp) 'mas-terminos)
    )))


;;Nombre de la funcion: termino?
;;Contrato(input and output): recibe una expresion y devuelve #t si
;;representa un termino y #f en caso contrario.
;;Proposito: identificar la representacion de un termino.
(define termino?
  (lambda(exp)
   (and (pair? exp) (equal? (car exp) 'termino)
    )))


;;Nombre de la funcion: coef-ent?
;;Contrato(input and output): recibe una expresion y devuelve #t si
;;representa un coeficiente entero y #f en caso contrario.
;;Proposito: identificar la representacion de un coeficiente entero.
(define coef-ent?
  (lambda(exp)
    (and (pair? exp) (equal? (car exp) 'coef-ent)
    )))


;;Nombre de la funcion: coef-rac?
;;Contrato(input and output): recibe una expresion y devuelve #t si
;;representa un coeficiente racional y #f en caso contrario.
;;Proposito: identificar la representacion de un coeficiente racional.
(define coef-rac?
  (lambda(exp)
   (and (pair? exp) (equal? (car exp) 'coef-rac)
    )))


;;Nombre de la funcion: expo-nat?
;;Contrato(input and output): recibe una expresion y devuelve #t si
;;representa un exponente natural y #f en caso contrario.
;;Proposito: identificar la representacion de un exponente natural.
(define expo-nat?
  (lambda(exp)
   (and (pair? exp) (equal? (car exp) 'expo-nat)
    )))


;;Extractores

;;Nombre de la funcion: poli->var
;;Contrato(input and output): recibe un polinomio y devuelve su variable.
;;Proposito: extraer la variable almacenada dentro de un polinomio.
(define poli->var ;;<polinomio>
  (lambda(exp)
    (cadr exp);halla segundo valor
    ))


;;Nombre de la funcion: poli->terms
;;Contrato(input and output): recibe un polinomio y devuelve su lista de terminos.
;;Proposito: extraer la lista de terminos almacenada dentro de un polinomio.
(define poli->terms ;;<polinomio> terminos
  (lambda(exp)
    (caddr exp);halla Tercer valor
    ))


;;Nombre de la funcion: nombre-var->s
;;Contrato(input and output): recibe una variable abstracta y devuelve
;;el simbolo que contiene.
;;Proposito: extraer el simbolo de una variable.
(define nombre-var->s ;;<variable>
  (lambda(exp)
    (cadr exp)
    ))


;;Nombre de la funcion: mas-terminos->term
;;Contrato(input and output): recibe una lista de terminos no vacia
;;y devuelve el primer termino.
;;Proposito: extraer el primer termino de una lista de terminos.
(define mas-terminos->term ;;<terminos> termino
  (lambda(exp)
    (cadr exp)
    ))


;;Nombre de la funcion: mas-terminos->resto
;;Contrato(input and output): recibe una lista de terminos no vacia
;;y devuelve el resto de la lista de terminos.
;;Proposito: extraer el resto de una lista de terminos.
(define mas-terminos->resto ;<terminos> resto
  (lambda(exp)
    (caddr exp)
    ))


;;Nombre de la funcion: termino->coef
;;Contrato(input and output): recibe un termino y devuelve su coeficiente.
;;Proposito: extraer el coeficiente de un termino.
(define termino->coef ;;<termino> coeficiente
  (lambda(exp)
    (cadr exp)
    ))


;;Nombre de la funcion: termino->expo
;;Contrato(input and output): recibe un termino y devuelve su exponente.
;;Proposito: extraer el exponente de un termino.
(define termino->expo ;;<termino> exponente
  (lambda(exp)
    (caddr exp)
    ))


;;Nombre de la funcion: coef-ent->n
;;Contrato(input and output): recibe un coeficiente entero y devuelve
;;el numero entero que contiene.
;;Proposito: extraer el valor entero de un coeficiente.
(define coef-ent->n ;;<coeficiente>numero entero n
  (lambda(exp)
    (cadr exp)
    ))


;;Nombre de la funcion: coef-rac->num
;;Contrato(input and output): recibe un coeficiente racional y devuelve
;;su numerador.
;;Proposito: extraer el numerador de un coeficiente racional.
(define coef-rac->num ;;<coeficiente> numerador
  (lambda(exp)
    (cadr exp)
    ))


;;Nombre de la funcion: coef-rac->den
;;Contrato(input and output): recibe un coeficiente racional y devuelve
;;su denominador.
;;Proposito: extraer el denominador de un coeficiente racional.
(define coef-rac->den ;;<coeficiente> denominador
  (lambda(exp)
    (caddr exp)
    ))


;;Nombre de la funcion: expo-nat->k
;;Contrato(input and output): recibe un exponente abstracto y devuelve
;;el numero natural que contiene.
;;Proposito: extraer el valor del exponente.
(define expo-nat->k ;;<exponente> obtengo exponente k
  (lambda(exp)
    (cadr exp)
    ))


;;Funciones Auxiliares
;;(A partir de aqui el codigo es identico al de polinomios-procedimientos.rkt,
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


;;=====================================================
;;Ejemplos de construccion y uso (representacion con listas)
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


;;Construccion de polinomios
;;1. (polinomio-cero 'x)
;;   => el polinomio cero en x
;;2. (insertar-termino (polinomio-cero 'x) 7 0)
;;   => el polinomio 7
;;3. (insertar-termino (polinomio-cero 'x) 3/2 1)
;;   => el polinomio (3/2)x
;;4. (insertar-termino (insertar-termino (polinomio-cero 'x) 7 0) 4 5)
;;   => 4x^5 + 7
;;5. p (el polinomio de arriba)
;;   => 4x^5 - (3/2)x^2 + 7


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