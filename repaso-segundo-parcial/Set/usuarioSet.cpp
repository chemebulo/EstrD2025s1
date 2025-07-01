#include <iostream>
#include "Set.h"
using namespace std;

// #################################################################################################################### \\

/* INTERFAZ DE SET:
    * Set emptyS()
    * bool isEmptyS(Set s)
    * bool belongsS(int x, Set s)
    * void AddS(int x, Set s)
    * void RemoveS(int x, Set s)
    * int sizeS(Set s)
    * LinkedList setToList(Set s)
    * void DestroyS(Set s)
*/

int main() {
    Set set1 = emptyS();
    Set set2 = emptyS();
    AddS(55, set1);
    cout << boolalpha << "Esta vacio el primer set:  " << isEmptyS(set1) << endl;
    cout << boolalpha << "Esta vacio el segundo set: " << isEmptyS(set2) << endl;
    AddS(10, set1);
    AddS(15, set1);
    AddS(20, set1);
    AddS(3, set2);
    AddS(2, set2);
    AddS(1, set2);
    cout << endl;
    cout << "La longitud del primer set es " << sizeS(set1) << "." << endl;
    cout << "La longitud del segundo set es " << sizeS(set2) << "." << endl;
    cout << endl;
    cout << boolalpha << "Existe el 10 en el primer set: " << belongsS(10, set1) << endl;
    cout << boolalpha << "Existe el 5 en el primer set: " << belongsS(5, set2) << endl;
    cout << endl;
    LinkedList xs = setToList(set1);
    LinkedList ys = setToList(set2);
    cout << "El primer elemento de la conversion a lista del primer set es " << head(xs) << "." << endl;
    cout << "El primer elemento de la conversion a lista del segundo set es " << head(ys) << "." << endl;
    cout << endl;
    DestroyS(set1);
    DestroyS(set2);
    cout << "La longitud del primer set es " << sizeS(set1) << ". Claramente no tiene sentido, porque fueron borrados." << endl;
    cout << "La longitud del segundo set es " << sizeS(set2) << ". Claramente no tiene sentido, porque fueron borrados." << endl;
    return 0;
}