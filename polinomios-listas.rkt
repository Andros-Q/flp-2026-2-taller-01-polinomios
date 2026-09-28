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


;;Area del Programador
(define polinomio-cero
  (lambda (variable)
    (eopl:error 'polinomio-cero "Sin implementar")))

(define insertar-termino
  (lambda (polinomio coeficiente exponente)
    (eopl:error 'insertar-termino "Sin implementar")))

(define coeficiente-de
  (lambda (polinomio exponente)
    (eopl:error 'coeficiente-de "Sin implementar")))

(define eliminar-termino
  (lambda (polinomio exponente)
    (eopl:error 'eliminar-termino "Sin implementar")))
