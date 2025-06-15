#include <iostream>
#include "QueueBFS.h"
#include "../tp-10/ArrayList.h"
using namespace std;

int sumarTBFS(Tree t){
// PROPÓSITO: Dado un árbol binario de enteros devuelve la suma entre sus elementos.
// COSTO: O(N).
// Siendo N la cantidad de elementos en el árbol, por cada recursión se realizan operaciones de costo constante, es por eso
// que el costo total de la función es lineal.
    if (t == NULL){
        return 0;
    }

    Queue q = emptyQ();
    Enqueue(t, q);

    int suma = 0;

    while (!isEmptyQ(q)) {
        Tree actual = firstQ(q);
        Dequeue(q);

        suma += actual->elem;

        if (actual->left != NULL)
            Enqueue(actual->left, q);

        if (actual->right != NULL)
            Enqueue(actual->right, q);
    }

    DestroyQ(q);
    return suma;
}

int sizeTBFS(Tree t){
// PROPÓSITO: Dado un árbol binario devuelve su cantidad de elementos, es decir, el tamaño del árbol (size en inglés).
// COSTO: O(N).
// Siendo N la cantidad de elementos en el árbol, por cada recursión se realizan operaciones de costo constante, es por eso
// que el costo total de la función es lineal.
    if (t == NULL){
        return 0;
    }

    Queue q = emptyQ();
    Enqueue(t, q);

    int size = 0;

    while (!isEmptyQ(q)) {
        Tree actual = firstQ(q);
        Dequeue(q);

        size += 1;

        if (actual->left != NULL)
            Enqueue(actual->left, q);

        if (actual->right != NULL)
            Enqueue(actual->right, q);
    }

    DestroyQ(q);
    return size;
}

bool perteneceTBFS(int e, Tree t){
// PROPÓSITO: Dados un elemento y un árbol binario devuelve True si existe un elemento igual a ese en el árbol.
// COSTO: O(N).
// Siendo N la cantidad de elementos en el árbol, por cada recursión se realizan operaciones de costo constante, es por eso
// que el costo total de la función es lineal.
    if (t == NULL){
        return false;
    }

    Queue q = emptyQ();
    Enqueue(t, q);

    while (!isEmptyQ(q)) {
        Tree actual = firstQ(q);
        Dequeue(q);

        if(actual->elem == e){
            DestroyQ(q);
            return true;
        }

        if (actual->left != NULL)
            Enqueue(actual->left, q);

        if (actual->right != NULL)
            Enqueue(actual->right, q);
    }

    DestroyQ(q);
    return false;
}

int aparicionesTBFS(int e, Tree t){
// PROPÓSITO: Dados un elemento e y un árbol binario devuelve la cantidad de elementos del árbol que son iguales a e.
// COSTO: O(N).
// Siendo N la cantidad de elementos en el árbol, por cada recursión se realizan operaciones de costo constante, es por eso
// que el costo total de la función es lineal.
    if (t == NULL){
        return 0;
    }

    Queue q = emptyQ();
    Enqueue(t, q);

    int apariciones = 0;

    while (!isEmptyQ(q)) {
        Tree actual = firstQ(q);
        Dequeue(q);

        if(actual->elem == e){
            apariciones += 1;
        }

        if (actual->left != NULL)
            Enqueue(actual->left, q);

        if (actual->right != NULL)
            Enqueue(actual->right, q);
    }

    DestroyQ(q);
    return apariciones;
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

ArrayList toListBFS(Tree t){
// PROPÓSITO: Dado un árbol devuelve una lista con todos sus elementos.
// COSTO: O(N).
// Siendo N la cantidad de elementos en el árbol, por cada recursión se realizan operaciones de costo constante, es por eso
// que el costo total de la función es lineal.
    ArrayList xs = newArrayList();

    if (t == NULL){
        return xs;
    }

    Queue q = emptyQ();
    Enqueue(t, q);

    while (!isEmptyQ(q)) {
        Tree actual = firstQ(q);
        Dequeue(q);

        add(actual->elem, xs);

        if (actual->left != NULL)
            Enqueue(actual->left, q);

        if (actual->right != NULL)
            Enqueue(actual->right, q);
    }

    DestroyQ(q);
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

/* INTERFAZ DE QUEUE BFS:
    * Queue emptyQ();
    * bool isEmptyQ(Queue q);
    * Tree firstQ(Queue q);
    * void Enqueue(Tree t, Queue q);
    * void Dequeue(Queue q);
    * int lengthQ(Queue q);
    * void MergeQ(Queue q1, Queue q2);
    * void DestroyQ(Queue q);
*/

int main(){
    Queue q1 = emptyQ();
    cout << boolalpha << "La Queue 1 se encuentra vacia: " << isEmptyQ(q1) << endl;
    DestroyQ(q1);
    Tree t1 = nodeT(10, emptyT(), emptyT());
    Queue q2 = emptyQ();
    Enqueue(t1, q2);
    cout << "La longitud de la Queue 2 es de " << lengthQ(q2) << "." << endl;
    DestroyQ(q2);
    Tree t2 = nodeT(20, emptyT(), emptyT());
    Tree t3 = nodeT(30, emptyT(), emptyT());
    Queue q3 = emptyQ();
    Enqueue(t2, q3);
    Enqueue(t3, q3);
    cout << "El primer elemento de la Queue 3 es el " << rootT(firstQ(q3)) << "." << endl;
    Dequeue(q3);
    cout << "El primer elemento de la Queue 3 es el " << rootT(firstQ(q3)) << "." << endl;
    DestroyQ(q3);
    Tree t4 = nodeT(40, emptyT(), emptyT());
    Tree t5 = nodeT(50, emptyT(), emptyT());
    Tree t6 = nodeT(60, emptyT(), emptyT());
    Queue q4 = emptyQ();
    Enqueue(t4, q4);
    Enqueue(t5, q4);
    Enqueue(t6, q4);
    Dequeue(q4);
    cout << "La longitud de la Queue 4 es de " << lengthQ(q4) << "." << endl;
    DestroyQ(q4);
    return 0;
}