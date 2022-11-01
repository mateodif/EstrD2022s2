#include "Fraccion.h"

// Propósito: construye una fraccion
// Precondición: el denominador no es cero
Fraccion consFraccion(int numerador, int denominador) {
  Fraccion f;
  f.numerador = numerador;
  f.denominador = denominador;
  return f;
}

// Propósito: devuelve el numerador
int numerador(Fraccion f){
    return f.numerador;
}

// Propósito: devuelve el denominador
int denominador(Fraccion f){
    return f.denominador;
}

// Propósito: devuelve el resultado de hacer la división
float division(Fraccion f){
    return f.numerador / f.denominador;
}

// Propósito: devuelve una fracción que resulta de multiplicar las fracciones
// (sin simplificar)
Fraccion multF(Fraccion f1, Fraccion f2){
    Fraccion f;
    f.numerador = f1.numerador * f2.numerador;
    f.denominador = f1.denominador * f2.denominador;
    return f;
}

// Propósito: devuelve una fracción que resulta
// de simplificar la dada por parámetro
int mayorDenominador(int a, int b) {
  int x = 0;
  while (b != 0) {
    x = a;
    a = b;
    b = x % b;
  }
  return a;
}

Fraccion simplificada(Fraccion p){
    int minimo = mayorDenominador(p.numerador, p.denominador);
    p.numerador = p.numerador / minimo;
    p.denominador = p.denominador / minimo;
    return p;
}

// Propósito: devuelve la primera componente
Fraccion sumF(Fraccion f1, Fraccion f2){
    Fraccion f;
    f.denominador = f1.denominador * f2.denominador;
    f.numerador = (f1.numerador * f2.denominador) + (f2.numerador * f1.denominador);
    return f;
}
