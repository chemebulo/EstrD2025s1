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
// int main() {
//     Par p = consPar(9, 94);
//     cout << "(" << p.x << ", " << p.y << ")" << endl;
//     return 0;
// }


// EJERCICIO 3.2:
int fst(Par p) {
    // PROPÓSITO: Devuelve la primera componente.
    int x = p.x;   
    return x;
}
// int main() {
//     int x = fst(consPar(9, 94));
//     cout << x << endl;
//     return 0;
// }


// EJERCICIO 3.3:
int snd(Par p){
    // PROPÓSITO: Devuelve la segunda componente.
    int y = p.y;   
    return y;
}
// int main() {
//     int y = snd(consPar(9, 94));
//     cout << y << endl;
//     return 0;
// }


// EJERCICIO 3.4:
int maxDelPar(Par p){
    // PROPÓSITO: Devuelve la mayor componente.
    int x = p.x;
    int y = p.y;
    if (x > y) {
        return x;
    } else {
        return y;
    }
}
// int main() {
//     int max = maxDelPar(consPar(9, 94));
//     cout << max << endl;
//     return 0;
// }


// EJERCICIO 3.5:
Par swap(Par p){
    // PROPÓSITO: Devuelve un par con las componentes intercambiadas.
    int x = p.x;
    int y = p.y;
    return consPar(y, x); 
}
// int main() {
//     Par nuevo = swap(consPar(40, 39));
//     cout << "(" << nuevo.x << ", " << nuevo.y << ")" << endl;
//     return 0;
// }


// EJERCICIO 3.6:
Par divisionYResto(int n, int m){
    // PROPÓSITO: Devuelve un par donde la primer componente es la división y la segunda el resto entre ambos números.
    int division = n / m;
    int resto = n % m;
    Par divYResto = consPar(division, resto);
    return divYResto;
}
// int main() {
//     Par resultado = divisionYResto(10, 3);
//     cout << "(" << resultado.x << ", " << resultado.y << ")" << endl;
//     return 0;
// }


// EJERCICIO 4:

// EJERCICIO 4.1 V1:
void printN(int n, string s){
// PROPÓSITO: Imprime n veces un string s.
    for (int i = 0; i < n; i++) {
        cout << s << endl;
    }
}

// EJERCICIO 4.1 V2:
void printNV2(int n, string s){
// PROPÓSITO: Imprime n veces un string s.
    if(n <= 0) return;
    cout << s << endl;
    printNV2(n - 1, s);
}
// int main() {
//     printNV2(5, "Hola");
//     return 0;
// }


// EJERCICIO 4.2 V1:
void cuentaRegresiva(int n){
// PROPÓSITO: Imprime los números desde n hasta 0, separados por saltos de línea.
    for (int i = n; i >= 0; i--) {
        cout << i << endl;
    }
}

// EJERCICIO 4.2 V2:
void cuentaRegresivaV2(int n){
// PROPÓSITO: Imprime los números desde n hasta 0, separados por saltos de línea.
    if(n < 0) return;
    cout << n << endl;
    cuentaRegresivaV2(n - 1);
}
// int main() {
//     cuentaRegresivaV2(5);
//     return 0;
// }


// EJERCICIO 4.3 V1:
void desdeCeroHastaN(int n){
// PROPÓSITO: Imprime los números de 0 hasta n, separados por saltos de línea.
    for(int i = 0; i <= n; i++) {
        cout << i << endl;
    }
}

// EJERCICIO 4.3 V2:
void desdeCeroHastaNV2(int n){
// PROPÓSITO: Imprime los números de 0 hasta n, separados por saltos de línea.
    if(n < 0) return;
    desdeCeroHastaNV2(n - 1);
    cout << n << endl;
}
// int main(){
//     desdeCeroHastaNV2(5);
//     return 0;
// }

// EJERCICIO 4.4 V1:
int mult(int n, int m){
// PROPÓSITO: Realiza la multiplicación entre dos números (sin utilizar la operación * de C++).
    int resultado = 0;
    bool negativo = false;

    if (b < 0) {
        b = -b;
        negativo = true;
    }

    for(int i = 0; i < m; i++) {
        resultado += n;
    }

    return resultado;
}
int main() {
    int res = mult(4, (-2));
    cout << res << endl;
    return 0;
}

// EJERCICIO 4.4 V2:
// int multV2(int n, int m){
// // PROPÓSITO: Realiza la multiplicación entre dos números (sin utilizar la operación * de C++).
// }


// EJERCICIO 4.5 V1:
// void primerosN(int n, string s) {
// // PROPÓSITO: Imprime los primeros n char del string s, separados por un salto de línea.
// // PRECONDICIÓN: El string tiene al menos n char.
// }

// EJERCICIO 4.5 V2:
// void primerosN(int n, string s) {
// // PROPÓSITO: Imprime los primeros n char del string s, separados por un salto de línea.
// // PRECONDICIÓN: El string tiene al menos n char.
// }


// EJERCICIO 4.6 V1:
// bool pertenece(char c, string s){
// // PROPÓSITO: Indica si un char c aparece en el string s.
// }

// EJERCICIO 4.6 V2:
// bool pertenece(char c, string s){
// // PROPÓSITO: Indica si un char c aparece en el string s.
// }


// EJERCICIO 4.7 V1:
// int apariciones(char c, string s){
// // PROPÓSITO: Devuelve la cantidad de apariciones de un char c en el string s.
// }

// EJERCICIO 4.7 V2:
// int apariciones(char c, string s){
// // PROPÓSITO: Devuelve la cantidad de apariciones de un char c en el string s.
// }


// EJERCICIO 5:

// EJERCICIO 5.1: