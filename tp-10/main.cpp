#include "ArrayList.h"
#include <iostream>

using namespace std;

// Devuelve la suma de todos los elementos.
int sumatoria(ArrayList xs) {
  int temp = 0;
  for (int i = 0; i < xs->cantidad; i++) {
    temp += get(i, xs);
  }
  return temp;
}

// Incrementa en uno todos los elementos.
void sucesores(ArrayList xs) {
  for (int i = 0; i < xs->cantidad; i++) {
    set(i, get(i, xs) + 1, xs);
  }
}

// Indica si el elemento pertenece a la lista.
bool pertenece(int x, ArrayList xs) {
  for (int i = 0; i < xs->cantidad; i++) {
    if (get(i, xs) == x)
      return true;
  }
  return false;
}

// Indica la cantidad de elementos iguales a x.
int apariciones(int x, ArrayList xs) {
  int temp = 0;
  for (int i = 0; i < xs->cantidad; i++) {
    if (get(i, xs) == x)
      temp++;
  }
  return temp;
}

// Crea una nueva lista a partir de la primera y la segunda (en ese orden).
ArrayList append(ArrayList xs, ArrayList ys) {
  ArrayList zs = newArrayList();
  for (int i = 0; i < lengthAL(xs); i++) {
    add(get(i, xs), zs);
  }
  for (int i = 0; i < lengthAL(ys); i++) {
    add(get(i, ys), zs);
  }
  return zs;
}

// Devuelve el elemento más chico de la lista.
int minimo(ArrayList xs) {
  int temp = get(0, xs);
  for (int i = 0; i < xs->cantidad; i++) {
    int el = get(i, xs);
    if (el > temp)
      temp = el;
  }
  return temp;
}

int main(int argc, char *argv[]) { return 0; }
