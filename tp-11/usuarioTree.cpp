#include <iostream>
#include <algorithm>
#include "Tree.h"
#include "../tp-10/ArrayList.h"
using namespace std;

int sumarT(Tree t){
// PROPÓSITO: Dado un árbol binario de enteros devuelve la suma entre sus elementos.
// COSTO: O(N).
// Siendo N la cantidad de elementos en el árbol, por cada N se realizan operaciones de costo constante, es
// por eso que el costo total de la función es lineal.
    if (isEmptyT(t)){
        return 0;
    } else {
        return rootT(t) + sumarT(left(t)) + sumarT(right(t));
    }
}

int sizeT(Tree t){
// PROPÓSITO: Dado un árbol binario devuelve su cantidad de elementos, es decir, el tamaño del árbol (size en inglés).
// COSTO: O(N).
// Siendo N la cantidad de elementos en el árbol, por cada N se realizan operaciones de costo constante, es
// por eso que el costo total de la función es lineal.
    if (isEmptyT(t)){
        return 0;
    } else {
        return 1 + sizeT(left(t)) + sizeT(right(t));
    }
}

bool perteneceT(int e, Tree t){
// PROPÓSITO: Dados un elemento y un árbol binario devuelve True si existe un elemento igual a ese en el árbol.
// COSTO: O(N).
// Siendo N la cantidad de elementos en el árbol, por cada N se realizan operaciones de costo constante, es
// por eso que el costo total de la función es lineal.
    if (isEmptyT(t)){
        return false;
    } else {
        return (rootT(t) == e) || perteneceT(e, left(t)) || perteneceT(e, right(t));
    }
}

int aparicionesT(int e, Tree t){
// PROPÓSITO: Dados un elemento e y un árbol binario devuelve la cantidad de elementos del árbol que son iguales a e.
// COSTO: O(N).
// Siendo N la cantidad de elementos en el árbol, por cada N se realizan operaciones de costo constante, es
// por eso que el costo total de la función es lineal.
    if (isEmptyT(t)){
        return 0;
    } else {
        if(rootT(t) == e){
            return 1 + aparicionesT(e, left(t)) + aparicionesT(e, right(t));
        } else {
            return aparicionesT(e, left(t)) + aparicionesT(e, right(t));
        }
    }
}

int heightT(Tree t){
// PROPÓSITO: Dado un árbol devuelve su altura.
// COSTO: O(N).
// Siendo N la cantidad de elementos en el árbol, por cada N se realizan operaciones de costo constante, es
// por eso que el costo total de la función es lineal.
    if (isEmptyT(t)){
        return 0;
    } else {
        return 1 + max(heightT(left(t)), heightT(right(t)));
    }
}

void AgregarToList(Tree t, ArrayList xs){
// PROPÓSITO: Dado un árbol y un ArrayList, devuelve el ArrayList con todos los elementos del árbol dado.
// COSTO: O(N).
// Siendo N la cantidad de elementos en el árbol, por cada N se realizan operaciones de costo constante, es
// por eso que el costo total de la función es lineal.
    if(!isEmptyT(t)){
        add(rootT(t), xs);
        AgregarToList(left(t), xs);
        AgregarToList(right(t), xs);
    }
}

ArrayList toList(Tree t){
// PROPÓSITO: Dado un árbol devuelve una lista con todos sus elementos.
// COSTO: O(N).
// Siendo N la cantidad de elementos en el árbol, por cada N se realizan operaciones de costo constante, es
// por eso que el costo total de la función es lineal.
    ArrayList xs = newArrayList();
    AgregarToList(t, xs);
    return xs;
}

void AgregarLeavesEn(Tree t, ArrayList xs){
// PROPÓSITO: Dado un árbol y un ArrayList, devuelve el ArrayList con todos los elementos de las hojas del árbol dado.
// COSTO: O(N).
// Siendo N la cantidad de elementos en el árbol, por cada N se realizan operaciones de costo constante, es
// por eso que el costo total de la función es lineal.
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
// COSTO: O(N).
// Siendo N la cantidad de elementos en el árbol, por cada N se realizan operaciones de costo constante, es
// por eso que el costo total de la función es lineal.
    ArrayList xs = newArrayList();
    AgregarLeavesEn(t, xs);
    return xs;
}

void AgregarLevelNEn(int n, Tree t, ArrayList xs){
// PROPÓSITO: Dado un árbol y un ArrayList, devuelve el ArrayList con todos los elementos del nivel dado.
// COSTO: O(N).
// Siendo N la cantidad de elementos en el árbol, por cada N se realizan operaciones de costo constante, es
// por eso que el costo total de la función es lineal.
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
// COSTO: O(N).
// Siendo N la cantidad de elementos en el árbol, por cada N se realizan operaciones de costo constante, es
// por eso que el costo total de la función es lineal.
    ArrayList xs = newArrayList();
    AgregarLevelNEn(n, t, xs);
    return xs;
}

/* INTERFAZ DE TREE:
    * Tree emptyT();
    * Tree nodeT(int elem, Tree left, Tree right);
    * bool isEmptyT(Tree t);
    * int rootT(Tree t);
    * Tree left(Tree t);
    * Tree right(Tree t);
*/

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