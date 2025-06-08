#include <iostream>
#include "Persona.h"
using namespace std;

/* INTERFAZ DE PERSONA:
     * Persona consPersona(string nombre, int edad);
     * string nombre(Persona p);
     * int edad(Persona p);
     * void crecer(Persona p);
     * void cambioDeNombre(string nombre, Persona p);
     * bool esMayorQueLaOtra(Persona p1, Persona p2);
     * Persona laQueEsMayor(Persona p1, Persona p2);
*/

int main() {
    Persona juan = consPersona("Juan", 40);
    Persona ricardo = consPersona("Ricardo", 27);
    Persona mai = consPersona("Mai", 33);
    cout << "El nombre de la primera persona es " << nombre(juan) <<    " y tiene " << edad(juan) << " anios." << endl;
    cout << "El nombre de la segunda persona es " << nombre(ricardo) << " y tiene " << edad(ricardo) << " anios." << endl;
    cout << "El nombre de la tercera persona es " << nombre(mai) <<     " y tiene " << edad(mai) << " anios." << endl;
    crecer(juan);
    crecer(mai);
    cout << "Paso un anio, y actualmente Juan tiene " << edad(juan) << " anios." << endl;
    cout << "Paso un anio, y actualmente Mai tiene " << edad(mai) << " anios." << endl;
    cambioDeNombre("Juan Carlos", juan);
    Persona mayor = laQueEsMayor(juan, ricardo);
    cout << "Juan se cambio el nombre a " << nombre(juan) << "." << endl;
    cout << boolalpha << "Es verdad que Mai es mas grande que Juan:    " << esMayorQueLaOtra(mai, juan) << endl;
    cout << boolalpha << "Es verdad que Ricardo es mas grande que Mai: " << esMayorQueLaOtra(ricardo, mai) << endl;
    cout << "La persona mas grande entre Juan Carlos y Ricardo es " << nombre(mayor) << " con "<< edad(mayor) << " anios." << endl;
    return 0;
}