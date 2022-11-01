#include "ArrayList.h"
#include "iostream"

using namespace std;

// Crea una lista con 0 elementos.
// Nota: empezar el array list con capacidad 16.
ArrayList newArrayList() {
  ArrayList xs = new ArrayListSt;
  xs->cantidad = 0;
  xs->capacidad = 16;
  xs->elementos = new int[16];
  return xs;
}

// Crea una lista con 0 elementos y una capacidad dada por parámetro.
ArrayList newArrayListWith(int capacidad) {
  ArrayList xs = new ArrayListSt;
  xs->cantidad = 0;
  xs->capacidad = capacidad;
  xs->elementos = new int[capacidad];
  return xs;
}

// Devuelve la cantidad de elementos existentes.
int lengthAL(ArrayList xs) { return xs->cantidad; }

// Devuelve el iésimo elemento de la lista.
int get(int i, ArrayList xs) {
  return i <= xs->capacidad ? (xs->elementos)[i] : -1; // -1 si se sale del arr
}

// Reemplaza el iésimo elemento por otro dado.
void set(int i, int x, ArrayList xs) { (xs->elementos)[i] = x; }

// Decrementa o aumenta la capacidad del array.
// Nota: en caso de decrementarla, se pierden los elementos del final de la
// lista.
void resize(int capacidad, ArrayList xs) {
  int *temp = new int[capacidad];
  int minCant = min(xs->cantidad, capacidad);
  for (int i = 0; i < minCant; i++) {
    temp[i] = (xs->elementos)[i];
  }
  delete xs->elementos;
  xs->elementos = temp;
  xs->capacidad = minCant;
}

// Agrega un elemento al final de la lista.
void add(int x, ArrayList xs) {
  if (xs->cantidad == xs->capacidad) {
    resize(xs->cantidad * 2, xs);
  }
  set(xs->cantidad, x, xs);
  xs->cantidad++;
}

// Borra el último elemento de la lista.
void remove(ArrayList xs) { max(0, xs->cantidad - 1); }
