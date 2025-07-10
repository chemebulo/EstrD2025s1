#include <iostream>
#include "Ronda.h"
using namespace std;

Ronda mkRonda(){
// PROPÓSITO: Crea una nueva ronda vacía.
// COSTO TIEMPO: O(1).
// COSTO ESPACIO: O(1).
    RondaStr* ronda = new RondaStr;
    ronda->current = NULL;
    return ronda;
}

int current(Ronda ronda){
// PROPÓSITO: Describe el valor del nodo actual de la ronda.
// COSTO TIEMPO: O(1).
// COSTO ESPACIO: O(1).
    return ronda->current->value;
}

void move(int steps, Ronda ronda){
// PROPÓSITO: Mueve el puntero del nodo actual hacia adelante la cantidad de veces dada.
// COSTO TIEMPO: O(length).
// COSTO ESPACIO: O(1).
    for(int i = 0; i < steps; i++){
        ronda->current = ronda->current->next;
    }
}

void insert(int value, Ronda ronda){
// PROPÓSITO: Añade un nuevo nodo con el valor dado a la ronda. No admite repetidos esta solución.
// COSTO TIEMPO: O(N).
// COSTO ESPACIO: O(1).

    if(ronda->current == NULL){
        // Caso que no hay ningún nodo en la ronda.
        RondaNode* node = new RondaNode;
        node->value = value;
        node->next = node;
        node->prev = node;
        ronda->current = node;
    } else {
        // Caso que hay al menos un nodo en la ronda.
        RondaNode* current = ronda->current->next;

        // Verifico que no existe actualmente el valor dado en la ronda.
        while(current != ronda->current && value != current->value){
            current = current->next;
        } 

        // Puede que haya dado la vuelta a la ronda, o no, me interesa saber si el valor de donde estoy parado no es el valor a ingresar. 
        if(value != current->value){
            // Si dio vuelta la ronda y el último nodo no tiene el valor dado, esto va a dar verdadero y se va a agregar el nuevo nodo.
            RondaNode* node = new RondaNode;
            node->value = value;
            node->next = current->next;
            node->prev = current;
    
            current->next->prev = node;
            current->next = node;
            ronda->current = node;
        }
    }
}

void remove(Ronda ronda){
// PROPÓSITO: Elimina el nodo actual de la ronda.
// COSTO TIEMPO: O(1).
// COSTO ESPACIO: O(1).
    RondaNode* current = ronda->current;

    if(current != NULL){
        if(current == current->next){ 
            // Caso que hay uno solo.
            ronda->current = NULL;
        } else { 
            // Caso que hay varios.
            current->prev->next = current->next;
            current->next->prev = current->prev;
            ronda->current = current->prev;
        }
    }
    
    delete current;
}

int length(Ronda ronda){
// PROPÓSITO: Describe la longitud de la ronda.
// COSTO TIEMPO: O(N).
// COSTO ESPACIO: O(1).
    int r = 0;
    RondaNode* current = ronda->current;
    RondaNode* start = current;

    if(current != NULL){
        while(current->next != start){
            r++;
            current = current->next;
        }
        r++;
    }

    return r;
}