#include <iostream>
#include "Entrenador.h"
using namespace std;

/* INTERFAZ DE ENTRENADOR:
    * Entrenador consEntrenador(string nombre, int cantidad, Pokemon* pokemon)
    * string nombreDeEntrenador(Entrenador e)
    * int cantidadDePokemon(Entrenador e)
    * int cantidadDePokemonDe(TipoDePokemon tipo, Entrenador e)
    * Pokemon pokemonNro(int i, Entrenador e)
    * bool leGanaATodos(Entrenador e1, Entrenador e2)
*/

int main() {
    Pokemon pikachu = consPokemon("Fuego");
    Pokemon charmander = consPokemon("Agua");
    Pokemon charizard = consPokemon("Planta");
    Pokemon magikarp = consPokemon("Agua");

    Pokemon* listaR = new Pokemon[4];
    listaR[0] = pikachu;
    listaR[1] = charmander;
    listaR[2] = charizard;
    listaR[3] = magikarp;
    
    Pokemon* listaN = new Pokemon[2];
    listaN[0] = charizard;
    listaN[1] = magikarp;
    
    Entrenador roman = consEntrenador("Roman", 4, listaR);
    Entrenador niko = consEntrenador("Niko", 2, listaN);

    cout << endl;
    cout << "El nombre del primer entrenador es " << nombreDeEntrenador(roman) << "." << endl;
    cout << "El nombre del segundo entrenador es " << nombreDeEntrenador(niko) << "." << endl;
    cout << endl;
    cout << "El entrenador Roman tiene " << cantidadDePokemon(roman) << " pokemons." << endl;
    cout << "El entrenador Niko tiene " << cantidadDePokemon(niko) << " pokemons." << endl;
    cout << endl;
    cout << "La cantidad de pokemons tipo Agua que tiene Roman es " << cantidadDePokemonDe("Agua", roman) << "." << endl;
    cout << "La cantidad de pokemons tipo Fuego que tiene Roman es " << cantidadDePokemonDe("Fuego", roman) << "." << endl;
    cout << "La cantidad de pokemons tipo Agua que tiene Niko es " << cantidadDePokemonDe("Agua", niko) << "." << endl;
    cout << "La cantidad de pokemons tipo Fuego que tiene Niko es " << cantidadDePokemonDe("Fuego", niko) << "." << endl;
    cout << endl;
    cout << "El pokemon numero 3 de Roman es de tipo " << tipoDePokemon(pokemonNro(3, roman)) << "." << endl;
    cout << "El pokemon numero 1 de Roman es de tipo " << tipoDePokemon(pokemonNro(1, roman)) << "." << endl; 
    cout << "El pokemon numero 1 de Niko es de tipo " << tipoDePokemon(pokemonNro(1, niko)) << "." << endl; 
    cout << "El pokemon numero 2 de Niko es de tipo " << tipoDePokemon(pokemonNro(2, niko)) << "." << endl; 
    cout << endl;
    cout << boolalpha << "Algun pokemon de Roman le gana a todos los de Niko: " << leGanaATodos(roman, niko) << endl;
    cout << boolalpha << "Algun pokemon de Niko le gana a todos los de Roman: " << leGanaATodos(niko, roman) << endl;
    return 0;
}