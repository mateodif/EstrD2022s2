#include <iostream>

using namespace std;

char* reverse(char word[]) {
  int l = 0;
  while (word[l] != 0) {
    l++;
  }
  char* result = new char[l+1];
  result[l] = 0;

  for (int i = 0; i < l; i++) {
    result[i] = word[(l - 1) - 1];
  }

  return result;
}


void reverseInPlace(char word[]) {
  int l = 0;
  while (word[l] != 0) {
    l++;
  }
  for(int i = 0; i < l/2; i++){
    char a = word[i];
    word[i] = word[(l - 1) - 1];
    word[(l - 1) - 1] = a;
  }
}

void desdeCeroHastaN(int n){
  if(n != 0){
    desdeCeroHastaN(n-1);
  }
  cout << n << "\n";
}

int apariciones(char c, string s){
  int l = s.size();
  int res = 0;
  for(int i = 0; i <= l; i++){
    if(s[i] == c){
      res++;
    }
  }
  return res;
}

// int apariciones2(char c, string s){
//   int n = 0;
//   if(c == s[0]){
//     n = 1;
//   }
//   return n + aparicione
// }

struct Fraccion {
  int numerador;
  int denominador;
};

Fraccion consFraccion(int numerador, int denominador){
  Fraccion f;
  f.numerador = numerador;
  f.denominador = denominador;
  return f;
}

int mayorDenominador(int a, int b){
  int x = 0;
  while(b != 0){
    x = a;
    a = b;
    b = x % b;
  }
  return a;
}

int main(int argc, char *argv[]) {
  int a = mayorDenominador(30, 28);
  cout << a << "\n";
  return 0;
}
