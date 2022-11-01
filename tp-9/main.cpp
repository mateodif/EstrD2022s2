#include "Par.h"
#include "Fraccion.h"
#include <iostream>

using namespace std;

// EJERCICIO 2

// Precondición: c1 < c2
void printFromTo(char c1, char c2) {
  for (int i = 0; c1 + i <= c2; i++) {
    cout << c1 + i << ", ";
  }
  cout << endl;
}
// printFromTo('a', 'e');
// => a, b, c, d,
// en realidad => 97, 98, 99, 100, 101,

// Precondición: n >= 0
int fc(int n) {
  int x = 1;
  while (n > 0) {
    x = x * n;
    n--;
  }
  return x;
}
// fc(5);
// => 120

// Precondición: n <= m
int ft(int n, int m) {
  if (n == m) {
    return n;
  }
  return n + ft(n + 1, m);
}
// ft(1, 5);
// => 1 + 2 + 3 + 4 + 5
// 15

// EJERCICIO 3

void mostrarPar(Par p) {
  cout << "Par(";
  cout << "x <- " << fst(p) << ", \n";
  cout << "    y <- " << snd(p) << ", \n";
  cout << ")" << endl;
}

void probarPar(Par p) {
  mostrarPar(p);
  cout << maxDelPar(p) << endl;
  mostrarPar(swap(p));
  mostrarPar(divisionYResto(10, 5));
}

// EJERCICIO 4

// Propósito: imprime n veces un string s.
void printNIter(int n, string s) {
  for (int i = 0; i < n; i++) {
    cout << s << endl;
  }
}

void printNRecur(int n, string s) {
  if (n > 0) {
    cout << s << endl;
    printNRecur(n - 1, s);
  }
}

// Propósito: imprime los números desde n hasta 0, separados por saltos de
// línea.
void cuentaRegresivaIter(int n) {
  while (n >= 0) {
    cout << n << endl;
    n--;
  }
}

void cuentaRegresivaRecur(int n) {
  if (n >= 0) {
    cout << n << endl;
    cuentaRegresivaRecur(n - 1);
  }
}

// Propósito: imprime los números de 0 hasta n, separados por saltos de línea.
void desdeCeroHastaNIter(int n) {
  for (int i = 0; i <= n; i++) {
    cout << i << endl;
  }
}

void desdeCeroHastaNRecur(int n) {
  if (n != 0) {
    desdeCeroHastaNRecur(n - 1);
  }
  cout << n << "\n";
}

// Propósito: realiza la multiplicación entre dos números (sin utilizar la
// operación * de C++).
int multIter(int n, int m) {
  int r;
  bool negar = (n < 0 && m > 0) || (n > 0 && m < 0);

  int a = abs(n);
  int b = abs(m);

  for (r = 0; b > 0; b--) {
    r = r + a;
  }

  return negar ? -r : r;
}

int multRecur(int n, int m) {
  bool negar = (n < 0 && m > 0) || (n > 0 && m < 0);

  int a = abs(n);
  int b = abs(m);
  int r = a;

  if(m == 0 || n == 0){
    return 0;
  } else if(m != 1){
    r = r + multRecur(r, b - 1);
  }

  return negar ? -r : r;
}

// Propósito: imprime los primeros n char del string s, separados por un salto
// de línea. Precondición: el string tiene al menos n char.
void primerosNIter(int n, string s) {
  for(int i = 0; i < n; i++){
    cout << s[i] << endl;
  }
}

void primerosNRecur(int n, string s){
  if(n > 0){
    primerosNRecur(n - 1, s);
  }
  cout << s[n] << endl;
}

// Propósito: indica si un char c aparece en el string s.
bool perteneceIter(char c, string s) {
  bool r = false;
  for(int i = 0; r == false && i < s.size(); i++){
    r = c == s[i];
  }
  return r;
}

bool perteneceRecur(char c, string s) {
  if(s.size() == 0){
    return false;
  }
  return c == s[0] || perteneceRecur(c, s.substr(1));
}

// Propósito: devuelve la cantidad de apariciones de un char c en el string s.
int aparicionesIter(char c, string s) {
  int l = s.size();
  int res = 0;
  for (int i = 0; i <= l; i++) {
    if (s[i] == c) {
      res++;
    }
  }
  return res;
}

int aparicionesRecur(char c, string s) {
  if(s.size() == 0){
    return false;
  }
  return (c == s[0]) + aparicionesRecur(c, s.substr(1));
}

int main(int argc, char *argv[]) {
  cout << aparicionesRecur('a', "casa") << endl;
  return 0;
}
