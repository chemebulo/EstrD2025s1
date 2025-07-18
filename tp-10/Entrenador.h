#include <iostream>
#include "Pokemon.h"
using namespace std;

struct EntrenadorSt {
    string nombre;
    Pokemon* pokemon;
    int cantPokemon;
};

typedef EntrenadorSt* Entrenador;

Entrenador consEntrenador(string nombre, int cantidad, Pokemon* pokemon);
// PROPÓSITO: Dado un nombre, una cantidad de pokémon, y un array de pokémon de ese tamaño, devuelve un entrenador.

string nombreDeEntrenador(Entrenador e);
// PROPÓSITO: Devuelve el nombre del entrenador.

int cantidadDePokemon(Entrenador e);
// PROPÓSITO: Devuelve la cantidad de pokémon que posee el entrenador.

int cantidadDePokemonDe(TipoDePokemon tipo, Entrenador e);
// PROPÓSITO: Devuelve la cantidad de pokémon de determinado tipo que posee el entrenador.

Pokemon pokemonNro(int i, Entrenador e);
// PROPÓSITO: Devuelve el pokémon número i de los pokémon del entrenador.
// PRECONDICIÓN: Existen al menos i−1 pokémon.

bool leGanaATodos(Entrenador e1, Entrenador e2);
// PROPÓSITO: Dados dos entrenadores, indica si para cada pokémon del segundo entrenador, el primero
//            posee al menos un pokémon que le gane.