#lang eopl
;Autores: Nombre1 Codigo1, Nombre2 Codigo2

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

(provide polinomio-cero insertar-termino coeficiente-de eliminar-termino)


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
