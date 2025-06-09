#include <iostream>
using namespace std;

typedef string TipoDePokemon;

struct PokeSt {
    TipoDePokemon tipo;
    int energia;
};

typedef PokeSt* Pokemon;

Pokemon consPokemon(TipoDePokemon tipo);
// PROPÓSITO: Dado un tipo devuelve un pokémon con 100% de energía.

TipoDePokemon tipoDePokemon(Pokemon p);
// PROPÓSITO: Devuelve el tipo de un pokémon.

int energia(Pokemon p);
// PROPÓSITO: Devuelve el porcentaje de energía.

void perderEnergia(int energia, Pokemon p);
// PROPÓSITO: Le resta energía al pokémon.

bool superaA(Pokemon p1, Pokemon p2);
// PROPÓSITO: Dados dos pokémon indica si el primero en base al tipo, es superior al segundo. 
//            Agua supera a fuego, fuego a planta y planta a agua. Y cualquier otro caso es falso.