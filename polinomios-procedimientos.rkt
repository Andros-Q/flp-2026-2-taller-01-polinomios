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
