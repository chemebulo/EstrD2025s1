#include <iostream>
#include "Pokemon.h"
using namespace std;

Pokemon consPokemon(TipoDePokemon tipo){
// PROPÓSITO: Dado un tipo devuelve un pokémon con 100% de energía.
    PokeSt* p = new PokeSt;
    p->tipo = tipo;
    p->energia = 100;
    return p;
}

TipoDePokemon tipoDePokemon(Pokemon p){
// PROPÓSITO: Devuelve el tipo de un pokémon.
    return p->tipo;
}

int energia(Pokemon p){
// PROPÓSITO: Devuelve el porcentaje de energía.
    return p->energia;
}

void perderEnergia(int energia, Pokemon p){
// PROPÓSITO: Le resta energía al pokémon.
    p->energia -= energia;
}

bool superaA(Pokemon p1, Pokemon p2){
// PROPÓSITO: Dados dos pokémon indica si el primero en base al tipo, es superior al segundo. 
//            Agua supera a fuego, fuego a planta y planta a agua. Y cualquier otro caso es falso.
    return ((p1->tipo == "Agua"   && p2->tipo == "Fuego")  || 
            (p1->tipo == "Fuego"  && p2->tipo == "Planta") ||
            (p1->tipo == "Planta" && p2->tipo == "Agua"));
}