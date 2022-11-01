#include "Par.h"
#include <iostream>

using namespace std;


// Proposito: construye un par
Par consPar(int x, int y){
    Par miPar;
    miPar.x = x;
    miPar.y = y;
    return miPar;
}

// Proposito: devuelve la primera componente
int fst(Par p){
    return p.x;
}

// Proposito: devuelve la segunda componente
int snd(Par p){
    return p.y;
}

// Propósito: devuelve la mayor componente
int maxDelPar(Par p){
    if(p.x > p.y){
        return p.x;
    }else{
        return p.y;
    }
}

// Proposito: devuelve un par con las componentes intercambiadas
Par swap(Par p){
    int miX = p.x;
    p.x = p.y;
    p.y = miX;
    return p;
}

Par swapInmutable(Par p){
    Par par;
    par.x = p.y;
    par.y = p.x;
    return par;
}

// Proposito: devuelve un par donde la primer componente
// es la división y la segunda el resto entre ambos números
Par divisionYResto(int n, int m){
    Par par;
    par.x = n / m;
    par.y = n % m;
    return par;
}
