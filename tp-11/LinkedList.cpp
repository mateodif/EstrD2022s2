#include "LinkedList.h"
#include "iostream"

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

// Agrega un elemento al principio de la lista.
void Cons(int x, LinkedList xs) {
  NodoL *n = new NodoL;
  n->elem = x;
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
  NodoL *temp = new NodoL;
  temp->elem = x;
  temp->siguiente = NULL;

  NodoL *actual = xs->primero;
  if(actual == NULL){
    xs->primero = temp;
  }

  for (int i = 0; i < xs->cantidad; i++) {
    actual = actual->siguiente;
  }
  actual = temp;
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
  ixs->current->elem = x;
}

// Pasa al siguiente elemento.
void Next(ListIterator ixs){
  ixs->current = ixs->current->siguiente;
}

// Indica si el recorrido ha terminado.
bool atEnd(ListIterator ixs){
  return ixs->current->siguiente == NULL;
}

// Libera la memoria ocupada por el iterador.
void DisposeIterator(ListIterator ixs){
  delete ixs;
}

// Libera la memoria ocupada por la lista.
void DestroyL(LinkedList xs){
  NodoL *temp = xs->primero;
  while (temp != NULL) {
    delete temp;
    temp = temp->siguiente;
  }
  delete xs;
}

void showLinkedList(LinkedList xs){
  ListIterator ixs = getIterator(xs);
  while(!atEnd(ixs)){
    std::cout << current(ixs) << " -> ";
    Next(ixs);
  }
  std::cout << "END" << std::endl;
}
