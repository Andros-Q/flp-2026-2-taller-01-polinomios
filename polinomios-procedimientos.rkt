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
