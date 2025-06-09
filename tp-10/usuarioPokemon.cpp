#include <iostream>
#include "Pokemon.h"
using namespace std;

/* INTERFAZ DE POKEMON:
    * Pokemon consPokemon(TipoDePokemon tipo)
    * TipoDePokemon tipoDePokemon(Pokemon p)
    * int energia(Pokemon p)
    * void perderEnergia(int energia, Pokemon p)
    * bool superaA(Pokemon p1, Pokemon p2)
*/

int main() {
    Pokemon pikachu = consPokemon("Agua");
    Pokemon charmander = consPokemon("Fuego");
    cout << "El tipo de Pikachu es " << tipoDePokemon(pikachu) << " y tiene " << energia(pikachu) << " de energia." << endl;
    cout << "El tipo de Charmander es " << tipoDePokemon(charmander) << " y tiene " << energia(charmander) << " de energia." << endl;
    perderEnergia(81, pikachu);
    perderEnergia(17, charmander);
    cout << "Pikachu perdio energia, por eso ahora tiene " << energia(pikachu) << " de energia." << endl;
    cout << "Charmander perdio energia, por eso ahora tiene " << energia(charmander) << " de energia." << endl;
    cout << boolalpha << "Pikachu supera a Charmander -> " << superaA(pikachu, charmander) << endl;
    return 0;
}