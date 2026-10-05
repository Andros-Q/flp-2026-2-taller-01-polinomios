#lang eopl
;Autores: Andres Felipe Quiceno gil Codigo:2477362
;Yonier Alejandro Vega Rojas Codigo:2477056
;Jhon Fabricio Hurtado Marin Codigo: 2459472
;Juan Esteban Aguirre Castañeda Codigo:2459676

;; Taller 1 — Polinomios dispersos.
;; Parte 4: la misma batería de pruebas sobre las tres representaciones.

(require rackunit)
(require (prefix-in listas: "polinomios-listas.rkt"))
(require (prefix-in procs:  "polinomios-procedimientos.rkt"))
(require (prefix-in dt:     "polinomios-datatypes.rkt"))

;; ============================================================
;; BATERIA GENERAL
;; ============================================================
;; Nombre de la funcion: bateria
;; Contrato(input and output): recibe las cuatro funciones de la interfaz
;; (polinomio-cero, insertar-termino, coeficiente-de y eliminar-termino)
;; de una representacion y no devuelve nada util, solo ejecuta las pruebas.
;; Proposito: correr exactamente las mismas pruebas sobre las tres
;; representaciones. Solo se compara con coeficiente-de porque con
;; procedimientos check-equal? no sirve (dos closures nunca son equal?).
;; Que el resultado sea igual en las tres es la evidencia de la unicidad.
(define bateria
  (lambda (cero ins coef elim)
    ;; p representa 4x^5 - (3/2)x^2 + 7
    (let ([p (ins (ins (ins (cero 'x) 7 0) -3/2 2) 4 5)])

      ;; ---------- coeficiente-de ----------
      ;; Coeficiente del mayor exponente, de uno intermedio y del menor.
      (check-equal? (coef p 5) 4)
      (check-equal? (coef p 2) -3/2)
      (check-equal? (coef p 0) 7)

      ;; ---------- polinomio nulo como caso base ----------
      ;; Consultar sobre el nulo da error.
      (check-exn #rx"no tiene termino"
                 (lambda () (coef (cero 'x) 0)))
      ;; Eliminar sobre el nulo da error.
      (check-exn #rx"no tiene termino"
                 (lambda () (elim (cero 'x) 3)))
      ;; Insertar sobre el nulo funciona.
      (check-equal? (coef (ins (cero 'x) 5 3) 3) 5)

      ;; ---------- insertar-termino ----------
      ;; Exponente nuevo en medio de dos existentes: no se pierde nada.
      (check-equal? (coef (ins p 9 3) 3) 9)
      (check-equal? (coef (ins p 9 3) 5) 4)
      (check-equal? (coef (ins p 9 3) 2) -3/2)
      ;; Exponente nuevo mayor que todos y menor que todos.
      (check-equal? (coef (ins p 6 8) 8) 6)
      (check-equal? (coef (ins (ins (cero 'x) 3 4) 5 2) 2) 5)
      ;; Exponente existente: los coeficientes se suman.
      (check-equal? (coef (ins p 1 2) 2) -1/2)
      (check-equal? (coef (ins (ins (cero 'x) 3 4) 5 4) 4) 8)
      ;; Suma de racionales con el mismo exponente.
      (check-equal? (coef (ins (ins (cero 'x) 1/2 4) 1/4 4) 4) 3/4)
      ;; La suma da cero: el termino desaparece y los demas siguen.
      (check-exn #rx"no tiene termino"
                 (lambda () (coef (ins p 3/2 2) 2)))
      (check-equal? (coef (ins p 3/2 2) 5) 4)
      (check-equal? (coef (ins p 3/2 2) 0) 7)
      ;; Cancelar el unico termino deja el polinomio sin terminos.
      (check-exn #rx"no tiene termino"
                 (lambda () (coef (ins (ins (cero 'x) 5 4) -5 4) 4)))
      ;; Coeficiente cero: el polinomio no cambia (exponente nuevo
      ;; mayor, nuevo menor y existente).
      (check-exn #rx"no tiene termino"
                 (lambda () (coef (ins p 0 9) 9)))
      (check-exn #rx"no tiene termino"
                 (lambda () (coef (ins p 0 1) 1)))
      (check-equal? (coef (ins p 0 2) 2) -3/2)
      (check-equal? (coef (ins p 0 9) 5) 4)

      ;; ---------- eliminar-termino ----------
      ;; Eliminar el de mayor exponente, uno intermedio y el menor.
      (check-exn #rx"no tiene termino" (lambda () (coef (elim p 5) 5)))
      (check-equal? (coef (elim p 5) 2) -3/2)
      (check-exn #rx"no tiene termino" (lambda () (coef (elim p 2) 2)))
      (check-equal? (coef (elim p 2) 5) 4)
      (check-equal? (coef (elim p 2) 0) 7)
      (check-exn #rx"no tiene termino" (lambda () (coef (elim p 0) 0)))
      (check-equal? (coef (elim p 0) 5) 4)
      ;; Eliminar el unico termino deja el polinomio sin terminos.
      (check-exn #rx"no tiene termino"
                 (lambda () (coef (elim (ins (cero 'x) 9 4) 4) 4)))

      ;; ---------- los tres casos de error ----------
      ;; Exponente negativo en insertar-termino.
      (check-exn #rx"exponente debe ser un entero no negativo"
                 (lambda () (ins p 5 -1)))
      ;; Exponente no registrado en coeficiente-de.
      (check-exn #rx"no tiene termino"
                 (lambda () (coef p 3)))
      ;; Exponente no registrado en eliminar-termino.
      (check-exn #rx"no tiene termino"
                 (lambda () (elim p 3)))

      ;; ---------- otras validaciones ----------
      ;; Coeficiente inexacto.
      (check-exn #rx"coeficiente debe ser un numero"
                 (lambda () (ins p 2.5 3)))
      ;; Exponente inexacto (2.0 no cuenta como entero exacto).
      (check-exn #rx"exponente debe ser un entero no negativo"
                 (lambda () (ins p 5 2.0))))))


;; ============================================================
;; PRUEBAS DE LA REPRESENTACION CON LISTAS
;; ============================================================

;; La misma bateria sobre listas.
(bateria listas:polinomio-cero
         listas:insertar-termino
         listas:coeficiente-de
         listas:eliminar-termino)

;; Con listas si se puede comparar la estructura completa con check-equal?.

;; polinomio-cero
(check-equal?
 (listas:polinomio-cero 'x)
 (listas:poli
  (listas:nombre-var 'x)
  (listas:sin-terminos)))

;; Insertar varios terminos conservando el orden decreciente.
(check-equal?
 (listas:insertar-termino
  (listas:insertar-termino
   (listas:polinomio-cero 'x)
   3
   4)
  5
  2)
 (listas:poli
  (listas:nombre-var 'x)
  (listas:mas-terminos
   (listas:termino
    (listas:coef-ent 3)
    (listas:expo-nat 4))
   (listas:mas-terminos
    (listas:termino
     (listas:coef-ent 5)
     (listas:expo-nat 2))
    (listas:sin-terminos)))))

;; Insertar un termino que queda en medio.
(check-equal?
 (listas:insertar-termino
  (listas:insertar-termino
   (listas:insertar-termino
    (listas:polinomio-cero 'x)
    3
    8)
   5
   2)
  7
  5)
 (listas:poli
  (listas:nombre-var 'x)
  (listas:mas-terminos
   (listas:termino
    (listas:coef-ent 3)
    (listas:expo-nat 8))
   (listas:mas-terminos
    (listas:termino
     (listas:coef-ent 7)
     (listas:expo-nat 5))
    (listas:mas-terminos
     (listas:termino
      (listas:coef-ent 5)
      (listas:expo-nat 2))
     (listas:sin-terminos))))))

;; Un coeficiente racional se guarda con numerador y denominador.
(check-equal?
 (listas:insertar-termino (listas:polinomio-cero 'x) -3/2 2)
 (listas:poli
  (listas:nombre-var 'x)
  (listas:mas-terminos
   (listas:termino
    (listas:coef-rac -3 2)
    (listas:expo-nat 2))
   (listas:sin-terminos))))

;; Si la suma da cero, el polinomio queda igual al polinomio cero.
(check-equal?
 (listas:insertar-termino
  (listas:insertar-termino
   (listas:polinomio-cero 'x)
   5
   4)
  -5
  4)
 (listas:polinomio-cero 'x))

;; Insertar coeficiente cero no modifica el polinomio.
(check-equal?
 (listas:insertar-termino
  (listas:insertar-termino
   (listas:polinomio-cero 'x)
   5
   4)
  0
  2)
 (listas:insertar-termino
  (listas:polinomio-cero 'x)
  5
  4))

;; Eliminar un termino intermedio.
(check-equal?
 (listas:eliminar-termino
  (listas:insertar-termino
   (listas:insertar-termino
    (listas:insertar-termino
     (listas:polinomio-cero 'x)
     3
     8)
    7
    5)
   5
   2)
  5)
 (listas:insertar-termino
  (listas:insertar-termino
   (listas:polinomio-cero 'x)
   3
   8)
  5
  2))

;; Eliminar el unico termino deja el polinomio cero.
(check-equal?
 (listas:eliminar-termino
  (listas:insertar-termino
   (listas:polinomio-cero 'x)
   9
   4)
  4)
 (listas:polinomio-cero 'x))

;; Los observadores responden bien sobre la representacion de listas.
(check-true (listas:poli? (listas:polinomio-cero 'x)))
(check-true (listas:sin-terminos? (listas:sin-terminos)))
(check-false (listas:mas-terminos? (listas:sin-terminos)))
(check-equal? (listas:coef-rac->den (listas:coef-rac -3 2)) 2)


;; ============================================================
;; PRUEBAS DE LA REPRESENTACION CON PROCEDIMIENTOS
;; ============================================================

;; La misma bateria, sin ningun cambio en las pruebas.
(bateria procs:polinomio-cero
         procs:insertar-termino
         procs:coeficiente-de
         procs:eliminar-termino)

;; Los observadores responden bien sobre la representacion de procedimientos.
(check-true (procs:poli? (procs:polinomio-cero 'x)))
(check-true (procs:sin-terminos? (procs:sin-terminos)))
(check-false (procs:mas-terminos? (procs:sin-terminos)))
(check-false (procs:poli? 5))
(check-equal? (procs:coef-rac->den (procs:coef-rac -3 2)) 2)
(check-equal?
 (procs:nombre-var->s (procs:poli->var (procs:polinomio-cero 'y)))
 'y)


;; ============================================================
;; PRUEBAS DE LA REPRESENTACION CON DATATYPES
;; ============================================================

;; La misma bateria otra vez.
(bateria dt:polinomio-cero
         dt:insertar-termino
         dt:coeficiente-de
         dt:eliminar-termino)

;; Los constructores del datatype se pueden usar directamente.
(check-true
 (dt:polinomio?
  (dt:poli (dt:nombre-var 'x)
           (dt:mas-terminos
            (dt:termino (dt:coef-rac -3 2) (dt:expo-nat 2))
            (dt:sin-terminos)))))
(check-false (dt:polinomio? 5))

;; ---------- sumar ----------
;; p = 4x^5 - (3/2)x^2 + 7
;; q = -4x^5 + (1/2)x^2 + 2x
(define p-dt
  (dt:insertar-termino
   (dt:insertar-termino
    (dt:insertar-termino (dt:polinomio-cero 'x) 7 0)
    -3/2 2)
   4 5))

(define q-dt
  (dt:insertar-termino
   (dt:insertar-termino
    (dt:insertar-termino (dt:polinomio-cero 'x) 2 1)
    1/2 2)
   -4 5))

;; Ejemplo del enunciado: el resultado es -x^2 + 2x + 7.
(check-equal? (dt:coeficiente-de (dt:sumar p-dt q-dt) 2) -1)
(check-equal? (dt:coeficiente-de (dt:sumar p-dt q-dt) 1) 2)
(check-equal? (dt:coeficiente-de (dt:sumar p-dt q-dt) 0) 7)
;; El termino x^5 se cancela.
(check-exn #rx"no tiene termino"
           (lambda () (dt:coeficiente-de (dt:sumar p-dt q-dt) 5)))

;; La suma es conmutativa en este ejemplo.
(check-equal? (dt:coeficiente-de (dt:sumar q-dt p-dt) 2) -1)
(check-equal? (dt:coeficiente-de (dt:sumar q-dt p-dt) 1) 2)

;; Sumar con el polinomio cero deja el polinomio igual.
(check-equal? (dt:coeficiente-de (dt:sumar p-dt (dt:polinomio-cero 'x)) 5) 4)
(check-equal? (dt:coeficiente-de (dt:sumar (dt:polinomio-cero 'x) p-dt) 2) -3/2)

;; Dos polinomios que se cancelan por completo: p + (-p).
(define menos-p-dt
  (dt:insertar-termino
   (dt:insertar-termino
    (dt:insertar-termino (dt:polinomio-cero 'x) -7 0)
    3/2 2)
   -4 5))

(check-exn #rx"no tiene termino"
           (lambda () (dt:coeficiente-de (dt:sumar p-dt menos-p-dt) 5)))
(check-exn #rx"no tiene termino"
           (lambda () (dt:coeficiente-de (dt:sumar p-dt menos-p-dt) 2)))
(check-exn #rx"no tiene termino"
           (lambda () (dt:coeficiente-de (dt:sumar p-dt menos-p-dt) 0)))

;; Sumar polinomios en variables distintas debe dar error.
(check-exn #rx"misma variable"
           (lambda ()
             (dt:sumar p-dt
                       (dt:insertar-termino (dt:polinomio-cero 'y) 1 1))))