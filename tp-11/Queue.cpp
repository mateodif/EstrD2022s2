#include "Queue.h"
#include "iostream"

/* Crea una lista vacía. */
/* Costo: O(1). */
Queue emptyQ() {
  Queue q = new QueueSt;
  q->cantidad = 0;
  q->primero = NULL;
  q->ultimo = NULL;
  return q;
}

/* Indica si la lista está vacía. */
/* Costo: O(1). */
bool isEmptyQ(Queue q) { return q->cantidad == 0; }

/* Devuelve el primer elemento. */
/* Costo: O(1). */
int firstQ(Queue q) { return q->primero->elem; }

/* Agrega un elemento al final de la cola. */
/* Costo: O(1). */
void Enqueue(int x, Queue q) {
  NodoQ *n = new NodoQ;
  n->elem = x;
  n->siguiente = NULL;

  q->ultimo->siguiente = n;
}

/* Quita el primer elemento de la cola. */
/* Costo: O(1). */
void Dequeue(Queue q) {
  delete q->primero;
  q->primero = q->primero->siguiente;
}

/* Devuelve la cantidad de elementos de la cola. */
/* Costo: O(1). */
int lengthQ(Queue q) { return q->cantidad; }

/* Anexa q2 al final de q1, liberando la memoria inservible de q2 en el proceso.
 */
/* Nota: Si bien se libera memoria de q2, no necesariamente la de sus nodos. */
/* Costo: O(1). */
void MergeQ(Queue q1, Queue q2) {
  q1->ultimo = q2->primero;
  delete q2;
}

/* Libera la memoria ocupada por la lista. */
/* Costo: O(n). */
void DestroyQ(Queue q) {
  while (!isEmptyQ(q)) {
    delete q->primero;
    Dequeue(q);
  }
  delete q;
}
