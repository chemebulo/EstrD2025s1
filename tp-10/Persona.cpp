#include <iostream>
#include "Persona.h"
using namespace std;

Persona consPersona(string nombre, int edad){
// PROPÓSITO: Devuelve a una persona nueva, con el nombre y la edad dados.
    RegistroDeP* p = new RegistroDeP;
    p->nombre = nombre;
    p->edad = edad;
    
    return p;
}

string nombre(Persona p){
// PROPÓSITO: Devuelve el nombre de una persona.
    return p->nombre;
}

int edad(Persona p){
// PROPÓSITO: Devuelve la edad de una persona.
    return p->edad;
}

void crecer(Persona p){
// PROPÓSITO: Aumenta en uno la edad de la persona.
    p->edad++;
}

void cambioDeNombre(string nombre, Persona p){
// PROPÓSITO: Modifica el nombre una persona.
    p->nombre = nombre;
}

bool esMayorQueLaOtra(Persona p1, Persona p2){
// PROPÓSITO: Dadas dos personas indica si la primera es mayor que la segunda.
    return p1->edad > p2->edad;
}

Persona laQueEsMayor(Persona p1, Persona p2){
// PROPÓSITO: Dadas dos personas devuelve a la persona que sea mayor.
    if (p1->edad > p2->edad){
        return p1;
    } else {
        return p2;
    }
}