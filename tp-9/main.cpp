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

int main(int argc, char *argv[]) {
  cout << "hola" << endl;
  for (int i = 0; i < argc; i++) {
    cout << "arg " << i << " " << argv[i] << endl;
  }

  char prueba[] = "prueba";

  cout << reverse(prueba) << endl;

  return 0;
}
