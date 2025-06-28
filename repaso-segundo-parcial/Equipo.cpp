#include <iostream>
#include "Equipo.h"
using namespace std;

struct EquipoSt {
    Seleccion seleccion;
    string grupo;
};

Equipo crearEquipo(Seleccion seleccion, string grupo){
    EquipoSt* e = new EquipoSt;
    e->seleccion = seleccion;
    e->grupo = grupo; 
    return e;
};

string nombreEquipo(Equipo e){
    return nombreDeSeleccion(e->seleccion);
};

string grupoEquipo(Equipo e){
    return e->grupo;
};

Seleccion seleccionEquipo(Equipo e){
    return e->seleccion;
};

string nombreDeSeleccion(Seleccion s){
    switch (s){
        case ESTADOSUNIDOS: return "Estados Unidos";
        case COSTARICA: return "Costa Rica";
        case COLOMBIA:  return "Colombia";
        case PARAGUAY:  return "Paraguay";
        case HAITI:     return "Haiti";
        case PERU:      return "Peru";
        case BRASIL:    return "Brasil";
        case ECUADOR:   return "Ecuador";
        case VENEZUELA: return "Venezuela";
        case MEXICO:    return "Mexico";
        case URUGUAY:   return "Uruguay";
        case JAMAICA:   return "Jamaica";
        case ARGENTINA: return "Argentina";
        case BOLIVIA:   return "Bolivia";
        case CHILE:     return "Chile";
        case PANAMA:    return "Panama";
        default: return "Desconocida";
    }
};