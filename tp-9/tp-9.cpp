#include <iostream>
using namespace std;

// EJERCICIO 1:

    // https://prnt.sc/-Bx38ROVy9Mo


// EJERCICIO 2:

void printFromTo(char c1, char c2) {
// PROPÓSITO: Imprime los caracteres desde c1 hasta c2 en orden ASCII, separados por comas.
// PRECONDICIÓN: c1 < c2
    for(int i = 0; c1 + i <= c2; i++) {
        cout << c1 + i << ", ";
    }
    cout << endl;
}


int fc(int n) {
// PROPÓSITO: Calcula y devuelve el factorial de n.
// PRECONDICIÓN: n >= 0
    int x = 1;
    while(n > 0) {
        x = x * n;
        n--;
    }
    return x;
}


int ft(int n, int m) {
// PROPÓSITO: Calcula y devuelve la suma de los enteros desde n hasta m.
// PRECONDICIÓN: n <= m
    if (n == m) {
        return n;
    }
    return n + ft(n+1, m);
}


// EJERCICIO 3:

struct Par {
    int x;
    int y;
};


// EJERCICIO 3.1:
Par consPar(int x, int y) {
    // PROPÓSITO: Construye un par.
    Par p;
    p.x = x;
    p.y = y;
    return p;
}

int main() {
    Par p = consPar(9, 94);
    std::cout << "(" << p.x << ", " << p.y << ")" << std::endl;
    return 0;
}

// EJERCICIO 3.2:
// int fst(Par p);
// PROPÓSITO: Devuelve la primera componente.


// EJERCICIO 3.3:
// int snd(Par p);
// PROPÓSITO: Devuelve la segunda componente.


// EJERCICIO 3.4:
// int maxDelPar(Par p);
// PROPÓSITO: Devuelve la mayor componente.


// EJERCICIO 3.5:
// Par swap(Par p);
// PROPÓSITO: Devuelve un par con las componentes intercambiadas.


// EJERCICIO 3.6:
// Par divisionYResto(int n, int m);
// PROPÓSITO: Devuelve un par donde la primer componente es la división y la segunda el resto entre ambos números.