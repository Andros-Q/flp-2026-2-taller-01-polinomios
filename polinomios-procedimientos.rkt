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

;; Importo las funciones del otro archivo
(require "polinomios-listas.rkt")

;;Interfaz:
;;Constructores
(define poli ;<polinomio>
  (lambda (var terms)
    (lambda (s)
      (cond
        [(= s 0) 'poli]
        [(= s 1) var]
        [(= s 2) terms]
        [else (eopl:error 'poli "selector invalido")]))))

(define nombre-var ;<variable>
  (lambda (sym)
    (lambda (s)
      (cond
        [(= s 0) 'nombre-var]
        [(= s 1) sym]
        [else (eopl:error 'nombre-var "selector invalido")]))))

(define sin-terminos ;<terminos> sin terminos
  (lambda ()
    (lambda (s)
      (cond
        [(= s 0) 'sin-terminos]
        [else (eopl:error 'sin-terminos "selector invalido")]))))

(define mas-terminos  ;<terminos> con terminos
  (lambda (term resto)
    (lambda (s)
      (cond
        [(= s 0) 'mas-terminos]
        [(= s 1) term]
        [(= s 2) resto]
        [else (eopl:error 'mas-terminos "selector invalido")]))))

(define termino ;<termino>
  (lambda (coef expo)
    (lambda (s)
      (cond
        [(= s 0) 'termino]
        [(= s 1) coef]
        [(= s 2) expo]
        [else (eopl:error 'termino "selector invalido")]))))

(define coef-ent ;<coeficiente> coeficiente entero
  (lambda (n)
    (lambda (s)
      (cond
        [(= s 0) 'coef-ent]
        [(= s 1) n]
        [else (eopl:error 'coef-ent "selector invalido")]))))

(define coef-rac ;<coeficiente> coeficiente racional
  (lambda (num den)
    (lambda (s)
      (cond
        [(= s 0) 'coef-rac]
        [(= s 1) num]
        [(= s 2) den]
        [else (eopl:error 'coef-rac "selector invalido")]))))

(define expo-nat ;<exponente> 
  (lambda (k)
    (lambda (s)
      (cond
        [(= s 0) 'expo-nat]
        [(= s 1) k]
        [else (eopl:error 'expo-nat "selector invalido")]))))
<<<<<<< Updated upstream
=======
 
 
;;Predicados
 
;;los predicados validan si la expresion corresponde al tipo esperado
;;en este caso, use procedure? exp para confirmar que es una clausula y tambien use equal? para
;;confirmar que se identifique con el contructor correstamente
 
(define poli?
  (lambda (exp)
    (and (procedure? exp) (equal? (exp 0) 'poli))))
 
(define nombre-var?
  (lambda (exp)
    (and (procedure? exp) (equal? (exp 0) 'nombre-var))))
 
(define sin-terminos?
  (lambda (exp)
    (and (procedure? exp) (equal? (exp 0) 'sin-terminos))))
 
(define mas-terminos?
  (lambda (exp)
    (and (procedure? exp) (equal? (exp 0) 'mas-terminos))))
 
(define termino?
  (lambda (exp)
    (and (procedure? exp) (equal? (exp 0) 'termino))))
 
(define coef-ent?
  (lambda (exp)
    (and (procedure? exp) (equal? (exp 0) 'coef-ent))))
 
(define coef-rac?
  (lambda (exp)
    (and (procedure? exp) (equal? (exp 0) 'coef-rac))))
 
(define expo-nat?
  (lambda (exp)
    (and (procedure? exp) (equal? (exp 0) 'expo-nat))))
 
;;Extractores
 
(define poli->var
  (lambda (p)
    (p 1)))   ;;obtenemos la variable del polinomio
 
(define poli->terms
  (lambda (p)
    (p 2)))   ;;obtenemos los términos del polinomio
 
(define nombre-var->s
  (lambda (v)
    (v 1)))   ;;obtenemos el símbolo de la variable
 
(define mas-terminos->term
  (lambda (mt)
    (mt 1)))  ;;obtenemos el primer término
 
(define mas-terminos->resto
  (lambda (mt)
    (mt 2)))  ;;obtenemos el resto de términos
 
(define termino->coef
  (lambda (t)
    (t 1)))   ;;obtenemos el coeficiente del término
 
(define termino->expo
  (lambda (t)
    (t 2)))   ;;obtenemos el exponente del término
 
(define coef-ent->n
  (lambda (c)
    (c 1)))   ;;obtenemos el número entero
 
(define coef-rac->num
  (lambda (c)
    (c 1)))   ;;obtenemos el numerador del racional
 
(define coef-rac->den
  (lambda (c)
    (c 2)))   ;;obtenemos el denominador del racional
 
(define expo-nat->k
  (lambda (e)
    (e 1)))   ;;obtenemos el valor del exponente
 
 
;;Funciones Auxiliares
 
 
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
;;Ejemplos de construccion y uso (representacion con procedimientos)
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


;;Construccion de datos con los constructores y uso de los observadores
;;(p = 4x^5 - (3/2)x^2 + 7 construido a mano, sin insertar-termino)
;;p = (poli (nombre-var 'x)
;;          (mas-terminos (termino (coef-ent 4) (expo-nat 5))
;;            (mas-terminos (termino (coef-rac -3 2) (expo-nat 2))
;;              (mas-terminos (termino (coef-ent 7) (expo-nat 0))
;;                (sin-terminos)))))
;;
;;Constructores
;;1. (poli (nombre-var 'x) (sin-terminos))
;;   => el polinomio cero en x
;;2. (termino (coef-ent 4) (expo-nat 5))
;;   => el termino 4x^5
;;3. (mas-terminos (termino (coef-ent 7) (expo-nat 0)) (sin-terminos))
;;   => la lista de terminos que solo tiene el termino 7
;;4. (poli (nombre-var 'x)
;;         (mas-terminos (termino (coef-rac -3 2) (expo-nat 2)) (sin-terminos)))
;;   => el polinomio -(3/2)x^2
;;5. p (el polinomio de arriba)
;;   => 4x^5 - (3/2)x^2 + 7
;;
;;Observadores: predicados
;;6. (poli? (polinomio-cero 'x))      => #t
;;7. (mas-terminos? (sin-terminos))   => #f
;;8. (coef-rac? (coef-ent 4))         => #f
;;
;;Observadores: extractores
;;9.  (nombre-var->s (poli->var (polinomio-cero 'y)))   => y
;;10. (coef-rac->den (coef-rac -3 2))                   => 2
;;11. (expo-nat->k (termino->expo (mas-terminos->term (poli->terms p))))
;;    => 5
;;12. (coef-ent->n
;;      (termino->coef
;;        (mas-terminos->term
;;          (mas-terminos->resto (mas-terminos->resto (poli->terms p))))))
;;    => 7


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
>>>>>>> Stashed changes
