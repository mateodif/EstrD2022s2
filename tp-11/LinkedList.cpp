#include "LinkedList.h"
#include "iostream"

using namespace std;

// Crea una lista vacía.
LinkedList nil() {
  LinkedList xs = new LinkedListSt;
  xs->primero = NULL;
  xs->cantidad = 0;
  return xs;
}

// Indica si la lista está vacía.
bool isEmpty(LinkedList xs) { return xs->cantidad == 0; }

// Devuelve el primer elemento.
int head(LinkedList xs) {
  if (!isEmpty(xs))
    return xs->primero->elem;
  return -1; // no existe el primero
}

// Aux. Crear nuevo nodo
NodoL *createNode(int x) {
  NodoL *node = new NodoL;
  node->elem = x;
  node->siguiente = NULL;
  return node;
}

// Agrega un elemento al principio de la lista.
void Cons(int x, LinkedList xs) {
  NodoL *n = new NodoL;
  n->elem = x;
  if (isEmpty(xs))
    n->siguiente = NULL;
  else
    n->siguiente = xs->primero;

  xs->primero = n;
  xs->cantidad++;
}

// Quita el primer elemento.
void Tail(LinkedList xs) {
  NodoL *temp = xs->primero;
  xs->primero = xs->primero->siguiente;
  xs->cantidad--;
  delete temp;
}

// Devuelve la cantidad de elementos.
int length(LinkedList xs) { return xs->cantidad; }

// Agrega un elemento al final de la lista.
void Snoc(int x, LinkedList xs) {
  NodoL *temp = xs->primero;
  while (temp != NULL) {
    temp = temp->siguiente;
  }
  temp->siguiente = createNode(x);
  xs->cantidad++;
}

// Apunta el recorrido al primer elemento.
ListIterator getIterator(LinkedList xs) {
    ListIterator ixs = new IteratorSt;
    ixs->current = xs->primero;
    return ixs;
}

// Devuelve el elemento actual en el recorrido.
int current(ListIterator ixs){
    return ixs->current->elem;
}

// Reemplaza el elemento actual por otro elemento.
void SetCurrent(int x, ListIterator ixs){
    NodoL *temp = xs->primero;
    ixs->current;
}

// Pasa al siguiente elemento.
void Next(ListIterator ixs);

// Indica si el recorrido ha terminado.
bool atEnd(ListIterator ixs);

// Libera la memoria ocupada por el iterador.
void DisposeIterator(ListIterator ixs);

// Libera la memoria ocupada por la lista.
void DestroyL(LinkedList xs);
