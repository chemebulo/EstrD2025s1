#include <iostream>
using namespace std;

struct RegistroDeP {
    string nombre;
    int edad;
};

typedef RegistroDeP* Persona;

Persona consPersona(string nombre, int edad);
// PROPÓSITO: Devuelve a una persona nueva, con el nombre y la edad dados.

string nombre(Persona p);
// PROPÓSITO: Devuelve el nombre de una persona.

int edad(Persona p);
// PROPÓSITO: Devuelve la edad de una persona.

void crecer(Persona p);
// PROPÓSITO: Aumenta en uno la edad de la persona.

void cambioDeNombre(string nombre, Persona p);
// PROPÓSITO: Modifica el nombre una persona.

bool esMayorQueLaOtra(Persona p1, Persona p2);
// PROPÓSITO: Dadas dos personas indica si la primera es mayor que la segunda.

Persona laQueEsMayor(Persona p1, Persona p2);
// PROPÓSITO: Dadas dos personas devuelve a la persona que sea mayor.