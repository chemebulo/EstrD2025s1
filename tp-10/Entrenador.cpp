#include <iostream>
#include "Entrenador.h"
using namespace std;

Entrenador consEntrenador(string nombre, int cantidad, Pokemon* pokemon){
// PROPÓSITO: Dado un nombre, una cantidad de pokémon, y un array de pokémon de ese tamaño, devuelve un entrenador.
    EntrenadorSt* e = new EntrenadorSt;
    e->nombre = nombre;
    e->pokemon = pokemon;
    e->cantPokemon = cantidad;
    return e;
}

string nombreDeEntrenador(Entrenador e){
// PROPÓSITO: Devuelve el nombre del entrenador.
    return e->nombre;
}

int cantidadDePokemon(Entrenador e){
// PROPÓSITO: Devuelve la cantidad de pokémon que posee el entrenador.
    return e->cantPokemon;
}

int cantidadDePokemonDe(TipoDePokemon tipo, Entrenador e){
// PROPÓSITO: Devuelve la cantidad de pokémon de determinado tipo que posee el entrenador.
    int cantidadEncontrada = 0;

    for(int i = 0; i < (e->cantPokemon); i++){
        if((tipoDePokemon(e->pokemon[i]) == tipo)){
            cantidadEncontrada++;
        }
    }

    return cantidadEncontrada;
}

Pokemon pokemonNro(int i, Entrenador e){
// PROPÓSITO: Devuelve el pokémon número i de los pokémon del entrenador.
// PRECONDICIÓN: Existen al menos i−1 pokémon.
    return e->pokemon[i-1];
}

bool leGanaATodos(Entrenador e1, Entrenador e2){
// PROPÓSITO: Dados dos entrenadores, indica si para cada pokémon del segundo entrenador, el primero
//            posee al menos un pokémon que le gane.
    for(int i = 0; i < (e2->cantPokemon); i++){
        bool algunoLeGana = false;

        for(int j = 0; j < (e1->cantPokemon); j++){
            if(superaA(e1->pokemon[j], e2->pokemon[i])){
                algunoLeGana = true;
                break;
            }
        }

        if (not algunoLeGana) {
            return false;
        }
    }

    return true;
}