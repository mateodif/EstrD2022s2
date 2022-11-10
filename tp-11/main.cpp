#include "../tp-10/ArrayList.h"
#include "LinkedList.h"
#include "Queue.h"
#include "Tree.h"
#include <iostream>

// LINKED LIST USUARIO

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

// ARBOLES

// Dado un árbol binario de enteros devuelve la suma entre sus elementos.
void inOrder(Tree t) {
  Queue q = emptyQ();
  NodeT *current = t;

  while (current != NULL || !isEmptyQ(q)) {
    while (current != NULL) {
      Enqueue(current->elem, q);
      current = current->left;
    }
    current = firstQ(q); // no puedo meter nodos en la queue
    Dequeue(q);

    std::cout << current->elem << " ";
    current = current->right;
  }
}

int sumarT(Tree t) {}

// Dado un árbol binario devuelve su cantidad de elementos, es decir, el tamaño
// del árbol (size en inglés).
int sizeT(Tree t) {}

// Dados un elemento y un árbol binario devuelve True si existe un elemento
// igual a ese en el árbol.
bool perteneceT(int e, Tree t) {}

// Dados un elemento e y un árbol binario devuelve la cantidad de elementos
// del árbol que son iguales a e.
int aparicionesT(int e, Tree t) {}

// Dado un árbol devuelve su altura.
int heightT(Tree t) {}

// Dado un árbol devuelve una lista con todos sus elementos.
ArrayList toList(Tree t) {}

// Dado un árbol devuelve los elementos que se encuentran en sus hojas.
ArrayList leaves(Tree t) {}

// Dados un número n y un árbol devuelve una lista con los nodos de nivel n
ArrayList levelN(int n, Tree t) {}

int main(int argc, char *argv[]) {
  Tree root = nodeT(1,
                    nodeT(2,
                          nodeT(4,
                                NULL, NULL),
                          NULL),
                    nodeT(5,
                          NULL,
                          NULL));

  inOrder(root);
  return 0;
}
