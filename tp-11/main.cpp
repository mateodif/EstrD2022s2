#include "LinkedList.h"
#include <iostream>

// Devuelve la suma de todos los elementos.
int sumatoria(LinkedList xs) {
  ListIterator ixs = getIterator(xs);
  int total = 0;
  while (!atEnd(ixs)) {
    total += current(ixs);
    Next(ixs);
  }
  return total;
}

// Incrementa en uno todos los elementos.
void Sucesores(LinkedList xs) {
  ListIterator ixs = getIterator(xs);
  int total = 0;
  while (!atEnd(ixs)) {
    SetCurrent(current(ixs) + 1, ixs);
    Next(ixs);
  }
}

// Indica si el elemento pertenece a la lista.
bool pertenece(int x, LinkedList xs) {
  ListIterator ixs = getIterator(xs);
  bool encontrado = false;
  while (!atEnd(ixs)) {
    if (current(ixs) == x)
      return true;
    Next(ixs);
  }
  return encontrado;
}

// Indica la cantidad de elementos iguales a x.
int apariciones(int x, LinkedList xs) {
  ListIterator ixs = getIterator(xs);
  int encontrados = 0;
  while (!atEnd(ixs)) {
    if (current(ixs) == x) {
      encontrados++;
    }
    Next(ixs);
  }
  return encontrados;
}

// Devuelve el elemento más chico de la lista.
int minimo(LinkedList xs) {
  ListIterator ixs = getIterator(xs);
  int minimo = current(ixs);
  while (!atEnd(ixs)) {
    if (minimo > current(ixs)) {
      minimo = current(ixs);
    }
    Next(ixs);
  }
  return minimo;
}

// Dada una lista genera otra con los mismos elementos, en el mismo orden.
// Nota: notar que el costo mejoraría si Snoc fuese O(1), ¿cómo podría
// serlo?
LinkedList copy(LinkedList xs) {
  ListIterator ixs = getIterator(xs);
  LinkedList nueva = nil();
  while (!atEnd(ixs)) {
    Snoc(current(ixs), xs);
    Next(ixs);
  }
  return nueva;
}

// Agrega todos los elementos de la segunda lista al final de los de la
// primera. La segunda lista se destruye. Nota: notar que el costo mejoraría
// si Snoc fuese O(1), ¿cómo podría serlo?
void Append(LinkedList xs, LinkedList ys) {
  ListIterator iys = getIterator(ys);
  while (!atEnd(iys)) {
    Snoc(current(iys), xs);
    Next(iys);
  }
  DisposeIterator(iys);
  DestroyL(ys);
}

int main(int argc, char *argv[]) {
  LinkedList xs = nil();
  Snoc(1, xs);
  Snoc(2, xs);
  Snoc(3, xs);
  showLinkedList(xs);
  return 0;
}
