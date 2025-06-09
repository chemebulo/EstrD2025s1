#include <iostream>
#include "Par.h"
using namespace std;

/* INTERFAZ DE PAR:
    * Par consPar(int x, int y)
    * int fst(Par p)
    * int snd(Par p)
    * int maxDelPar(Par p)
    * Par swap(Par p)
    * Par divisionYResto(int n, int m);
*/

int main() {
    Par p= consPar(10, 3);
    cout << "Par:                (" << fst(p) << ", " << snd(p) << ")" << endl;
    cout << "Primer componente:  " << fst(p) << endl;
    cout << "Segundo componente: " << snd(p) << endl;
    cout << "Maximo del par:     " << maxDelPar(p) << endl;
    Par pi = swap(p);
    cout << "Par invertido:      (" << fst(pi) << ", " << snd(pi) << ")" << endl;
    p = divisionYResto(fst(p), snd(p));
    cout << "Dividir primero por segundo: Cociente -> " << fst(p) << endl;
    cout << "Dividir primero por segundo: Resto    -> " << snd(p) << endl;
    return 0;
}