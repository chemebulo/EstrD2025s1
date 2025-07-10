#include <iostream>
#include "Pokemon.h"
using namespace std;

// #################################################################################################################### \\

Pokemon crearPokemon(string nombre){
// PROPÓSITO: Describe un pókemon con 100 de vida y con el nombre dado.
// COSTO TIEMPO: O(1).
// COSTO ESPACIO: O(1).
    PokeStr* pokemon = new PokeStr;
    pokemon->vida = 100;
    pokemon->nombre = nombre;
    return pokemon;
}

void restarVida(Pokemon p){
// PROPÓSITO: Le resta 10 de vida al pókemon dado.
// COSTO TIEMPO: O(1).
// COSTO ESPACIO: O(1).
    if(p->vida >= 10){
        p->vida -= 10;
    } else {
        p->vida = 0;
    }
}

void cambiarNombre(Pokemon p, string nombre){
// PROPÓSITO: Le cambia el nombre al pókemon dado por el string dado.
// COSTO TIEMPO: O(1).
// COSTO ESPACIO: O(1).
    p->nombre = nombre;
}

bool estaVivo(Pokemon p){
// PROPÓSITO: Indica si el pókemon dado se encuentra vivo.
// COSTO TIEMPO: O(1).
// COSTO ESPACIO: O(1).
    return p->vida > 0;
}

string getNombre(Pokemon p){
// PROPÓSITO: Describe el nombre del pókemon dado.
// COSTO TIEMPO: O(1).
// COSTO ESPACIO: O(1).
    return p->nombre;
}

int getVida(Pokemon p){
// PROPÓSITO: Describe la vida del pókemon dado.
// COSTO TIEMPO: O(1).
// COSTO ESPACIO: O(1).
    return p->vida;
}

void luchar(Pokemon p, Pokemon r){
// PROPÓSITO: Dos pókemon dados luchan entre sí, restándole vida al que tenga un nombre de menor rango.
// COSTO TIEMPO: O(1).
// COSTO ESPACIO: O(1).
    if(p->nombre >= r->nombre){
        restarVida(r);
    } else {
        restarVida(p);
    }
}

void lucharN(int n, Pokemon p, Pokemon r){
// PROPÓSITO: Dos pókemon dados luchan entre sí la cantidad de veces dadas.
// COSTO TIEMPO: O(N). Siendo n el número dado por parámetro.
// COSTO ESPACIO: O(1).
    for(int i = 0; i < n && estaVivo(p) && estaVivo(r); i++){
        luchar(p, r);
    }
}

void destruir(Pokemon p){
// PROPÓSITO: Destruye al pókemon dado.
// COSTO TIEMPO: O(1).
// COSTO ESPACIO: O(1).
    delete p;
}

// #################################################################################################################### \\