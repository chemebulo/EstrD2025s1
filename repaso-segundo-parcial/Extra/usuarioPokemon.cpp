#include <iostream>
#include "Pokemon.h"
using namespace std;

// #################################################################################################################### \\

/* INTERFAZ PÓKEMON:
    * Pokemon crearPokemon(string nombre);
    * void restarVida(Pokemon& p);
    * void cambiarNombre(Pokemon& p, string nombre);
    * bool estaVivo(Pokemon p);
    * string getNombre(Pokemon p);
    * int getVida(Pokemon p);
    * void luchar(Pokemon& p, Pokemon& r);
    * void lucharN(int n, Pokemon& p, Pokemon& r);
    * void destruir(Pokemon& p);
*/

// #################################################################################################################### \\

int main(){
    Pokemon pikachu = crearPokemon("Pikachu");
    Pokemon charmander = crearPokemon("Charmander");
    Pokemon magikarp = crearPokemon("Magikarp");
    cout << "La vida de Pikachu actualmente es:    " << getVida(pikachu) << "." << endl;    
    cout << "La vida de Charmander actualmente es: " << getVida(charmander) << "." << endl;
    cout << "La vida de Magikarp actualmente es:   " << getVida(magikarp) << "." << endl;    
    restarVida(pikachu);
    restarVida(pikachu);
    restarVida(magikarp);
    cout << endl;
    cout << "La vida de Pikachu actualmente es:    " << getVida(pikachu) << "." << endl;    
    cout << "La vida de Charmander actualmente es: " << getVida(charmander) << "." << endl;
    cout << "La vida de Magikarp actualmente es:   " << getVida(magikarp) << "." << endl;    
    cout << endl;
    cout << boolalpha << "Se encuentra vivo Pikachu:    " << estaVivo(pikachu) << "." << endl;
    cout << boolalpha << "Se encuentra vivo Charmander: " << estaVivo(charmander) << "." << endl;
    cout << boolalpha << "Se encuentra vivo Magikarp:   " << estaVivo(magikarp) << "." << endl;
    cout << endl;
    cambiarNombre(pikachu, "Juan Carlos");
    cout << "Hackearon el sistema, ahora Pikachu se llama: " << getNombre(pikachu) << "." << endl;
    cambiarNombre(pikachu, "Pikachu");
    cout << "Todo volvio a la normalidad, Pikachu ahora se vuelve a llamar: " << getNombre(pikachu) << "." << endl;
    cout << endl;
    luchar(pikachu, charmander);
    luchar(charmander, magikarp);
    luchar(pikachu, magikarp);
    cout << boolalpha << "El rango de Pikachu es mayor que el de Charmander:  " << (getNombre(pikachu) >= getNombre(charmander)) << "." << endl;
    cout << boolalpha << "El rango de Charmander es mayor que el de Magikarp: " << (getNombre(charmander) >= getNombre(magikarp)) << "." << endl;
    cout << boolalpha << "El rango de Pikachu es mayor que el de Magikarp:    " << (getNombre(pikachu) >= getNombre(magikarp)) << "." << endl;
    cout << endl;
    cout << "La vida de Pikachu actualmente es:    " << getVida(pikachu) << "." << endl;    
    cout << "La vida de Charmander actualmente es: " << getVida(charmander) << "." << endl;
    cout << "La vida de Magikarp actualmente es:   " << getVida(magikarp) << "." << endl;    
    cout << endl;
    lucharN(5, pikachu, charmander);
    lucharN(5, charmander, magikarp);
    lucharN(5, pikachu, magikarp);
    cout << "La vida de Pikachu actualmente es:    " << getVida(pikachu) << "." << endl;    
    cout << "La vida de Charmander actualmente es: " << getVida(charmander) << "." << endl;
    cout << "La vida de Magikarp actualmente es:   " << getVida(magikarp) << "." << endl;    
    cout << endl;
    destruir(charmander);
    cout << "El nombre de Pikachu es:    " << getNombre(pikachu) << "." << endl;
    cout << "El nombre de Charmander es: " << getNombre(charmander) << "." << endl;
    cout << "El nombre de Magikarp es:   " << getNombre(magikarp) << "." << endl;
    return 0;
}