#include <iostream>
#include "BinaryTree.h"
#include "../tp-10/ArrayList.h"
using namespace std;

// #################################################################################################################### \\

int sumarT(Tree t){
// PROPÓSITO: Dado un árbol binario de enteros devuelve la suma entre sus elementos.
// COSTO TIEMPO: O(N).
// COSTO ESPACIO: O(N).
    int resultado = 0;

    if (!isEmptyT(t)){
        resultado += rootT(t) + sumarT(left(t)) + sumarT(right(t));
    }

    return resultado;
}

int sizeT(Tree t){
// PROPÓSITO: Dado un árbol binario devuelve su cantidad de elementos, es decir, el tamaño del árbol (size en inglés).
// COSTO TIEMPO: O(N).
// COSTO ESPACIO: O(N).
    int resultado = 0;

    if (!isEmptyT(t)){
        resultado += 1 + sizeT(left(t)) + sizeT(right(t));
    }

    return resultado;
}

bool perteneceT(int e, Tree t){
// PROPÓSITO: Dados un elemento y un árbol binario devuelve True si existe un elemento igual a ese en el árbol.
// COSTO TIEMPO: O(N).
// COSTO ESPACIO: O(N).
    bool encontrado = false;

    if (!isEmptyT(t)){
        encontrado = (rootT(t) == e) || perteneceT(e, left(t)) || perteneceT(e, right(t));
    }

    return encontrado;
}

int aparicionesT(int e, Tree t){
// PROPÓSITO: Dados un elemento e y un árbol binario devuelve la cantidad de elementos del árbol que son iguales a e.
// COSTO TIEMPO: O(N).
// COSTO ESPACIO: O(N).
    int apariciones = 0;

    if (!isEmptyT(t)){
        
        if((rootT(t) == e)){
            apariciones++;
        }  

        apariciones += aparicionesT(e, left(t)) + aparicionesT(e, right(t));
    }

    return apariciones;
}

int heightT(Tree t){
// PROPÓSITO: Dado un árbol devuelve su altura.
// COSTO TIEMPO: O(N).
// COSTO ESPACIO: O(N).
    int altura = 0;

    if (!isEmptyT(t)){
        altura = 1 + max(heightT(left(t)), heightT(right(t)));
    }

    return altura;
}

void AgregarToList(Tree t, ArrayList xs){
// PROPÓSITO: Dado un árbol y un ArrayList, devuelve el ArrayList con todos los elementos del árbol dado.
// COSTO TIEMPO: O(N).
// COSTO ESPACIO: O(N).
    if(!isEmptyT(t)){
        add(rootT(t), xs);
        AgregarToList(left(t), xs);
        AgregarToList(right(t), xs);
    }
}

ArrayList toList(Tree t){
// PROPÓSITO: Dado un árbol devuelve una lista con todos sus elementos.
// COSTO TIEMPO: O(N).
// COSTO ESPACIO: O(N).
    ArrayList xs = newArrayList();
    AgregarToList(t, xs);
    return xs;
}

void AgregarLeavesEn(Tree t, ArrayList xs){
// PROPÓSITO: Dado un árbol y un ArrayList, devuelve el ArrayList con todos los elementos de las hojas del árbol dado.
// COSTO TIEMPO: O(N).
// COSTO ESPACIO: O(N).
    if(!isEmptyT(t)){
        if(left(t) == NULL && right(t) == NULL){
            add(rootT(t), xs);
        }
        AgregarLeavesEn(left(t), xs);
        AgregarLeavesEn(right(t), xs);
    }
}

ArrayList leaves(Tree t){
// PROPÓSITO: Dado un árbol devuelve los elementos que se encuentran en sus hojas.
// COSTO TIEMPO: O(N).
// COSTO ESPACIO: O(N).
    ArrayList xs = newArrayList();
    AgregarLeavesEn(t, xs);
    return xs;
}

void AgregarLevelNEn(int n, Tree t, ArrayList xs){
// PROPÓSITO: Dado un árbol y un ArrayList, devuelve el ArrayList con todos los elementos del nivel dado.
// COSTO TIEMPO: O(N).
// COSTO ESPACIO: O(N).
    if(!isEmptyT(t)){
        if(n == 0){
            add(rootT(t), xs);
            return;
        }
        AgregarLevelNEn(n-1, left(t), xs);
        AgregarLevelNEn(n-1, right(t), xs);
    }
}

ArrayList levelN(int n, Tree t){
// PROPÓSITO: Dados un número n y un árbol devuelve una lista con los nodos de nivel n.
// COSTO TIEMPO: O(N).
// COSTO ESPACIO: O(N).
    ArrayList xs = newArrayList();
    AgregarLevelNEn(n, t, xs);
    return xs;
}

// #################################################################################################################### \\

/* INTERFAZ DE TREE:
    * Tree emptyT();
    * Tree nodeT(int elem, Tree left, Tree right);
    * bool isEmptyT(Tree t);
    * int rootT(Tree t);
    * Tree left(Tree t);
    * Tree right(Tree t);
*/

// #################################################################################################################### \\

int main(){
    Tree et = emptyT();
    Tree t1 = nodeT(7, et, et);
    Tree t2 = nodeT(9, et, et);
    Tree t3 = nodeT(6, t1, t2);
    Tree t4 = nodeT(11, et, et);
    Tree t5 = nodeT(5, t3, t4);
    ArrayList al = toList(t5);
    cout << "El largo del ArrayList es de " << lengthAL(al) << "." << endl;
    cout << "El primer elemento del array es " << get(0, al) << "." << endl;
    cout << "El segundo elemento del array es " << get(1, al) << "." << endl;
    cout << "El tercer elemento del array es " << get(2, al) << "." << endl;
    cout << "El cuarto elemento del array es " << get(3, al) << "." << endl;
    cout << "El quinto elemento del array es " << get(4, al) << "." << endl;
    return 0;
}

// #################################################################################################################### \\