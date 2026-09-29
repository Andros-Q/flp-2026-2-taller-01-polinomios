#lang eopl
;Autores: Nombre1 Codigo1, Nombre2 Codigo2

;; Taller 1 — Polinomios dispersos.
;; Parte 1: representación basada en listas.
;;
;; Interfaz del TAD. Cada función va comentada con su nombre, su contrato
;; (entrada -> salida) y su propósito, y ninguna recorre la lista de términos
;; más de una vez ni la ordena al final.
;;
;;   polinomio-cero    : symbol -> polinomio
;;   insertar-termino  : polinomio x coeficiente x exponente -> polinomio
;;   coeficiente-de    : polinomio x exponente -> coeficiente
;;   eliminar-termino  : polinomio x exponente -> polinomio

(provide polinomio-cero insertar-termino coeficiente-de eliminar-termino)

;;Interfaz:
;;Constructores
(define poli  ;<polinomio>
  (lambda(var terms)
    (list 'poli var terms)
    ))

(define nombre-var ;<variable>
  (lambda(s)
    (list 'nombre-var s)
    ))

(define sin-terminos ;<terminos> sin terminos
  (lambda()
    (list 'sin-terminos)
    ))

(define mas-terminos ;<terminos> con terminos
  (lambda(term resto)
    (list 'mas-terminos term resto)
    ))

(define termino ;<termino>
  (lambda(coef expo)
    (list 'termino coef expo)
    ))

(define coef-ent ;<coeficiente> coeficiente entero
  (lambda(n)
    (list 'coef-ent n)
    ))

(define coef-rac ;<coeficiente> coeficiente racional
  (lambda(num den)
    (list 'coef-rac num den)
    ))

(define expo-nat ;<exponente> 
  (lambda(k)
    (list 'expo-nat k)
    ))

;;Predicados
;;validar si en efecto la expresion pertenece a ese tipo de dato

  ;;Se pone pair con el objetivo de evitar que car de error
   ;;si la expresion dada no llega a ser una lista
(define poli?  
  (lambda(exp) ;;Confirmo con pair que en efecto es una lista y luego que la expresion en efecto coincida con el tipo de dato que quiero confirmar
    (and (pair? exp) (equal? (car exp) 'poli)
    )))

(define sin-terminos?
  (lambda(exp)
   (and (pair? exp) (equal? (car exp) 'sin-terminos)
    )))

(define mas-terminos?
  (lambda(exp)
   (and (pair? exp) (equal? (car exp) 'mas-terminos)
    )))

(define termino?
  (lambda(exp)
   (and (pair? exp) (equal? (car exp) 'termino)
    )))

(define coef-ent?
  (lambda(exp)
    (and (pair? exp) (equal? (car exp) 'coef-ent)
    )))

(define coef-rac?
  (lambda(exp)
   (and (pair? exp) (equal? (car exp) 'coef-rac)
    )))

(define expo-nat?
  (lambda(exp)
   (and (pair? exp) (equal? (car exp) 'expo-nat)
    )))


;;Extractores  
(define poli->var ;;<polinomio> 
  (lambda(exp)
    (cadr exp);halla segundo valor
    ))

(define poli->terms ;;<polinomio> terminos
  (lambda(exp)
    (caddr exp);halla Tercer valor
    ))

(define nombre-var->s ;;<variable>
  (lambda(exp)
    (cadr exp)
    ))

(define mas-terminos->term;;<terminos> termino
  (lambda(exp)
    (cadr exp)
    ))

(define mas-terminos->resto;<terminos> resto
  (lambda(exp)
    (caddr exp)
    ))

(define termino->coef;;<termino> coeficiente
  (lambda(exp)
    (cadr exp)
    ))

(define termino->expo;;<termino> exponente
  (lambda(exp)
    (caddr exp)
    ))

(define coef-ent->n ;;<coeficiente>numero entero n
  (lambda(exp)
    (cadr exp)
    ))

(define coef-rac->num ;;<coeficiente> numerador
  (lambda(exp)
    (cadr exp)
    ))

(define coef-rac->den ;;<coeficiente> denominador
  (lambda(exp)
    (caddr exp)
    ))

(define expo-nat->k;;<exponente> obtengo exponente k
  (lambda(exp)
    (cadr exp)
    ))

;;Funciones Auxiliares

;;Nombre de la funcion: coef-abstracto->concreto
;;Contrato(input and output): le entra un coeficiente abstracto(termino->coef term)
;;y devuelve el coeficiente concreto es decir un coeficiente entero o raciona(numerador/denominador)en este caso
;;Proposito: poder convertir un termino abstracto a un termino concreto con el objetivo de mostrar la solucion en forma concreta
;;y tambien se creo la funcion con el proposito de segmentar mejor el codigo y no dejar todo el codigo en una sola funcion.
(define coef-abstracto->concreto
  (lambda (exp)
    (cond
      [(coef-ent? exp) (coef-ent->n exp)]
      [(coef-rac? exp) (/ (coef-rac->num exp)
                          (coef-rac->den exp))]
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
  (lambda(terms expo)
    (cond
      [(sin-terminos? terms)(eopl:error
       "Lo siento: el polinomio
       no tiene termino con ese exponete ingresado") ]
      [else
       (let (   ;;Funcion auxiliar dentro de funcion buscar
              [term (mas-terminos->term terms)]  ;Define variables que se van a ligar en la zona exterior
              [resto (mas-terminos->resto terms)]
              [expt (termino->expo(mas-terminos->term terms))]
              )
          (cond
            [(= (expo-nat->k expt) expo)
             (coef-abstracto->concreto
              (termino->coef term))]
            [(> expo (expo-nat->k expt)) (eopl:error
       "Lo siento: el polinomio
       no tiene termino con ese exponete ingresado")]
            [else (buscar-coeficiente resto expo)]
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
       (poli(nombre-var variable) (sin-terminos))]
      [else (eopl:error "la variable debe ser un simbolo")]
    )))

(display (polinomio-cero 'y));Salida esperada
(display "\n");salto de linea

(define insertar-termino
  (lambda (polinomio coeficiente exponente)
    (eopl:error 'coeficiente-de "Sin implementar")
    ))

;;Nombre de la funcion: coeficiente-de
;;Contrato(input and output): le entra un polinomio y un exponente
;;y devuelve un coeficiente del polinomio asociado al exponente establecido
;;Proposito: Poder obtener adecuadamente el coeficiente que esta asociado ala potencia ingresada anterior mente
;;ejecutandose la logica pesada en la funcion buscar-coeficiente
(define coeficiente-de
  (lambda (polinomio exponente)
     (buscar-coeficiente (poli->terms polinomio) exponente)
    ))

(define eliminar-termino
  (lambda (polinomio exponente)
    (eopl:error 'eliminar-termino "Sin implementar")))

;;Ejemplo poli  p=4x^5+(-3/2)x^2+7 para hacer pruebas
(define p (poli (nombre-var 'x) (mas-terminos
 (termino(coef-ent 4) (expo-nat 5))
 (mas-terminos(termino (coef-rac -3 2) (expo-nat 2))
   (mas-terminos(termino (coef-ent 7)
     (expo-nat 0)) (sin-terminos)
              )))))