# Taller 1 — Polinomios dispersos

Fundamentos de Interpretación y Compilación de Lenguajes de Programación
Escuela de Ingeniería de Sistemas y Computación, Universidad del Valle
Carlos Andrés Delgado Saavedra

Un TAD polinomio disperso construido tres veces con la misma interfaz: con
listas, con procedimientos y con `define-datatype`. El enunciado completo,
con la especificación, las partes y las rúbricas, está publicado en el Campus
Virtual. Este repositorio es el fork de trabajo del grupo.

## Integrantes

| Nombre completo | Código | Correo institucional |
|---|---|---|
| Andres Felipe Quiceno Gil | 2477362 | quiceno.andres@correounivalle.edu.co |
| Yonier Alejandro Vega Rojas | 2477056 | yonier.vega@correounivalle.edu.co |
| Jhon Fabricio Hurtado Marin | 2459472 | hurtado.jhoan@correounivalle.edu.co |
| Juan Esteban Aguirre Castañeda | 2459676 | juan.esteban.aguirre@correounivalle.edu.co |

## Cómo se entrega

1. **Se trabaja sobre un fork de este repositorio.** El grupo hace fork, lo
   deja público y trabaja ahí. Una entrega que no sea fork de este
   repositorio pierde el 30 % de la nota.
2. **El enlace del fork se pega en la actividad del taller en el Campus
   Virtual.** Nada más: ni archivos, ni comprimidos, ni código por correo.
3. **Se califica el último commit anterior a la fecha y hora de cierre.** Lo
   que se suba después no se tiene en cuenta.
4. Solo archivos de texto: código `.rkt` en la raíz e informes `.md` en
   `docs/`. Fórmulas en LaTeX y diagramas en Mermaid dentro del Markdown; no
   se aceptan PDF, DOCX ni imágenes.
5. Las primeras líneas de cada `.rkt` llevan los autores:
   `;Autores: Nombre1 Codigo1, Nombre2 Codigo2`.
6. El historial de commits se revisa para ver el aporte de cada integrante.
   Los cambios de puro formato o de comentarios no cuentan.

## Cómo está organizado

```
polinomios-listas.rkt            Parte 1: representación basada en listas
polinomios-procedimientos.rkt    Parte 2: representación basada en procedimientos
polinomios-datatypes.rkt         Parte 3: representación con datatypes, más la suma
pruebas-polinomios.rkt           Parte 4: pruebas con rackunit sobre las tres
docs/informe-correccion.md       Parte 5: informe de corrección
docs/informe-ast.md              Parte 6: informe de árboles de sintaxis abstracta
```

Los archivos de implementación exportan sus funciones con `provide` y no
contienen pruebas; el archivo de pruebas las importa con `require`.

## Cómo se corre

```bash
racket pruebas-polinomios.rkt
```

Si todas las pruebas pasan, no imprime nada. Si alguna falla, muestra un
mensaje `FAILURE` con la ubicación de la prueba.
