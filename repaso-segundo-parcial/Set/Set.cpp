#include <iostream>
#include "Set.h"
using namespace std;

// #################################################################################################################### \\

Set emptyS(){
// PROPÓSITO: Crea un conjunto vacío.
// COSTO TIEMPO: O(1).
    // Siendo de costo constante, ya que solamente se reserva memoria en la heap y se asignan valores a los campos del registro creado.
// COSTO ESPACIO: O(1).
    // Siendo de costo constante, ya que solamente se utiliza un frame en la memoria stack en cualquier caso.
    SetSt* s = new SetSt;
    s->cantidad = 0;
    s->primero = NULL;
    return s;
}

bool isEmptyS(Set s){
// PROPÓSITO: Indica si el conjunto está vacío.
// COSTO TIEMPO: O(1).
    // Siendo de costo constante, ya que solamente se accede a un campo del registro.
// COSTO ESPACIO: O(1).
    // Siendo de costo constante, ya que solamente se utiliza un frame en la memoria stack en cualquier caso.
    return s->cantidad == 0;
}

bool belongsS(int x, Set s){
// PROPÓSITO: Indica si el elemento pertenece al conjunto.
// COSTO TIEMPO: O(n).
    // Siendo de costo lineal, ya que por cada elemento del conjunto se realizan comparaciones y una asignación del valor
    // la variable "temp", todas de costo constante. 
// COSTO ESPACIO: O(1).
    // Siendo de costo constante, ya que solamente se utiliza un frame en la memoria stack en cualquier caso.
    NodoS* temp = s->primero;

    while(temp != NULL && temp->elem != x){
        temp = temp->siguiente;
    }

    return temp != NULL && temp->elem == x;
}

void AddS(int x, Set s){
// PROPÓSITO: Agrega un elemento al conjunto.
// COSTO TIEMPO: O(n).
    // Siendo de costo lineal, ya que por cada elemento del conjunto se realizan comparaciones y una asignación del valor
    // la variable "temp", todas de costo constante.
// COSTO ESPACIO: O(1).
    // Siendo de costo constante, ya que solamente se utiliza un frame en la memoria stack en cualquier caso.
    NodoS* temp = s->primero;

    
    while(temp != NULL && temp->elem != x){
        temp = temp->siguiente;
    }
    
    if(temp == NULL){
        NodoS* nuevo = new NodoS;
        nuevo->elem = x;
        nuevo->siguiente = s->primero;

        s->primero = nuevo;
        s->cantidad++;
    }
}

void RemoveS(int x, Set s){
// PROPÓSITO: Quita un elemento dado.
// COSTO TIEMPO: O(n).
    // Siendo de costo lineal, ya que por cada elemento del conjunto se realizan comparaciones y una asignación del valor
    // la variable "temp", todas de costo constante.
// COSTO ESPACIO: O(1).
    // Siendo de costo constante, ya que solamente se utiliza un frame en la memoria stack en cualquier caso.
    NodoS* temp = s->primero;

    if(temp != NULL && temp->elem == x){
        s->primero = temp->siguiente;
        delete temp;
        s->cantidad--;
        return;
    }

    while(temp != NULL && temp->siguiente != NULL){
        if(temp->siguiente->elem == x){
            NodoS* borrar = temp->siguiente;
            temp->siguiente = temp->siguiente->siguiente;
            delete borrar;
            s->cantidad--;
            return;
        }
        temp = temp->siguiente;
    }
}

int sizeS(Set s){
// PROPÓSITO: Devuelve la cantidad de elementos.
// COSTO TIEMPO: O(1).
    // Siendo de costo constante, ya que solamente se accede a un campo del registro.
// COSTO ESPACIO: O(1).
    // Siendo de costo constante, ya que solamente se utiliza un frame en la memoria stack en cualquier caso.
    return s->cantidad;
}

LinkedList setToList(Set s){
// PROPÓSITO: Devuelve una lista con los elementos del conjunto.
// COSTO TIEMPO: O(n^2).
    // Siendo de costo cuadrático, ya que por cada elemento del conjunto se utiliza la operación "Snoc" de costo lineal.
// COSTO ESPACIO: O(1).
    // Siendo de costo constante, ya que solamente se utiliza un frame en la memoria stack en cualquier caso.
    NodoS* temp = s->primero;
    LinkedList xs = nil();

    while(temp != NULL){
        Snoc(temp->elem, xs);
        temp = temp->siguiente;
    }

    return xs;
}

void DestroyS(Set s){
// PROPÓSITO: Libera la memoria ocupada por el conjunto.
// COSTO TIEMPO: O(n).
    // Siendo de costo lineal, ya que por cada elemento del conjunto se realiza una comparación y una asignación de la variable
    // "temp", ambas de costo constante.
// COSTO ESPACIO: O(1).
    // Siendo de costo constante, ya que solamente se utiliza un frame en la memoria stack en cualquier caso.
    NodoS* temp = s->primero;

    while(temp != NULL){
        NodoS* borrar = temp;
        temp = temp->siguiente;
        delete borrar;
    }

    delete s;
}

// #################################################################################################################### \\