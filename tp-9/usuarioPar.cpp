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
    Par p= consPar(9, 94);
    cout << "(" << fst(p) << ", " << snd(p) << ")" << endl;
    cout << "Primer componente = " << fst(p) << endl;
    cout << "Segundo componente = " << snd(p) << endl;
    cout << "Maximo del par = " << maxDelPar(p) << endl;
    Par pr = swap(p);
    cout << "Par revertido = (" << fst(pr) << ", " << snd(pr) << ")" << endl;
    p = divisionYResto(fst(p), snd(p));
    cout << "Cociente y resto de dividir 1ro por 2do = Cociente -> " << fst(p) << " Resto -> " << snd(p) << endl;
    return 0;
}