#include "Persona.h"
#include <iostream>

using namespace std;

struct PersonaSt {
  string nombre;
  int    edad;
};

// Devuelve el puntero de la Persona nueva
Persona consPersona(string nombre, int edad){
  PersonaSt* p = new PersonaSt;
  p->nombre = nombre;
  p->edad = edad;
  return p;
}

// Devuelve el nombre de una persona
string nombre(Persona p){
  return p->nombre;
}

// Devuelve la edad de una persona
int edad(Persona p){
  return p->edad;
}

// Aumenta en uno la edad de la persona.
void crecer(Persona p){
  p->edad++;
}

// Modifica el nombre una persona.
void cambioDeNombre(string nombre, Persona p){
  p->nombre = nombre;
}

// Dadas dos personas indica si la primera es mayor que la segunda.
bool esMayorQueLaOtra(Persona p1, Persona p2){
  return p1->edad > p2->edad;
}

//Dadas dos personas devuelve a la persona que sea mayor.
Persona laQueEsMayor(Persona p1, Persona p2){
  return esMayorQueLaOtra(p1, p2) ? p1 : p2;
}

void showPersona(Persona p) {
  cout << "Persona[" << p << "](";
  cout << "nombre <- \"" << p->nombre << "\", ";
  cout << "edad <- " << p->edad;
  cout << ")" << endl;
}
