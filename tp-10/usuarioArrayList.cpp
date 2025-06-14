#include <iostream>
#include "ArrayList.h"
using namespace std;

int sumatoria(ArrayList xs){
// PROPÓSITO: Devuelve la suma de todos los elementos.
    int resultado = 0;

    for(int i = 0; i < lengthAL(xs); i++){
        resultado += get(i, xs);
    }

    return resultado;
}

void sucesores(ArrayList xs){
// PROPÓSITO: Incrementa en uno todos los elementos.
    for(int i = 0; i < lengthAL(xs); i++){
        set(i, get(i, xs) + 1, xs);
    }
}

bool pertenece(int x, ArrayList xs){
// PROPÓSITO: Indica si el elemento pertenece a la lista.
    for(int i = 0; i < lengthAL(xs); i++){
        if(get(i, xs) == x){
            return true;
        }
    }

    return false;
}

int apariciones(int x, ArrayList xs){
// PROPÓSITO: Indica la cantidad de elementos iguales a x.
    int resultado = 0;

    for(int i = 0; i < lengthAL(xs); i++){
        if(get(i, xs) == x){
            resultado++;
        }
    }

    return resultado;
}

ArrayList append(ArrayList xs, ArrayList ys){
// PROPÓSITO: Crea una nueva lista a partir de la primera y la segunda (en ese orden).
    int totalCapacidad = lengthAL(xs) + lengthAL(ys);
    ArrayList nuevaArray = newArrayListWith(totalCapacidad);

    for(int i = 0; i < lengthAL(xs); i++){
        add(get(i, xs), nuevaArray);
    }

    for(int i = 0; i < lengthAL(ys); i++){
        add(get(i, ys), nuevaArray);
    }

    return nuevaArray;
}

int minimo(ArrayList xs){
// PROPÓSITO: Devuelve el elemento más chico de la lista.
// PRECONDICIÓN: La lista no puede estar vacía.
    int minimo = get(0, xs);

    for(int i = 0; i < lengthAL(xs); i++){
        int actual = get(i, xs);

        if(actual < minimo){
            minimo = actual;
        }
    }

    return minimo;
}

/* INTERFAZ DE ARRAY LIST:
    * ArrayList newArrayList()
    * ArrayList newArrayListWith(int capacidad)
    * int lengthAL(ArrayList xs)
    * int get(int i, ArrayList xs)
    * void set(int i, int x, ArrayList xs)
    * void resize(int capacidad, ArrayList xs)
    * void add(int x, ArrayList xs)
    * void remove(ArrayList xs)
*/

int main() {
    ArrayList array1 = newArrayList();
    ArrayList array2 = newArrayListWith(4);

    cout << endl;
    cout << "La cantidad de elementos en el primer array es " << lengthAL(array1) << "." << endl;
    cout << "La cantidad de elementos en el segundo array es " << lengthAL(array2) << "." << endl;

    add(5, array1);
    add(2, array1);
    add(55, array2);
    add(7, array2);
    add(99, array2);
    add(2025, array2);

    cout << endl;
    cout << "La cantidad de elementos en el primer array es " << lengthAL(array1) << "." << endl;
    cout << "La cantidad de elementos en el segundo array es " << lengthAL(array2) << "." << endl;

    remove(array2);
    resize(1, array1);

    cout << endl;
    cout << "La cantidad de elementos en el primer array es " << lengthAL(array1) << "." << endl;
    cout << "La cantidad de elementos en el segundo array es " << lengthAL(array2) << "." << endl;

    cout << endl;
    cout << "El elemento numero 1 en el primer array es " << get(0, array1) << "." << endl;
    set(0, 2044, array1);
    cout << "El elemento numero 1 en el primer array es " << get(0, array1) << "." << endl;
    cout << "El elemento numero 2 en el segundo array es " << get(1, array2) << "." << endl;

    cout << endl;
    cout << "La sumatoria del primer array es " << sumatoria(array1) << "." << endl;
    cout << "La sumatoria del segundo array es " << sumatoria(array2) << "." << endl;

    sucesores(array1);
    sucesores(array2);

    cout << endl;
    cout << "Ahora el array son todos los sucesores, entonces el elemento numero 1 en el primer array es " << get(0, array1) << "." << endl;
    cout << "Ahora el array son todos los sucesores, entonces el elemento numero 2 en el segundo array es " << get(1, array2) << "." << endl;

    cout << endl;
    cout << boolalpha << "Existe el numero 2045 en el primer array: " << pertenece(2045, array1) << "." << endl;
    cout << boolalpha << "Existe el numero 777 en el segundo array: " << pertenece(777, array2) << "." << endl;

    cout << endl;
    cout << "El numero 2045 en el primer array aparece " << apariciones(2045, array1) << " veces." << endl;
    cout << "El numero 2025 en el primer array aparece " << apariciones(2025, array2) << " veces." << endl;

    cout << endl;
    ArrayList array1y2 = append(array1, array2);
    cout << "La cantidad de elementos en el primer array es de " << lengthAL(array1) << " elementos." << endl; 
    cout << "La cantidad de elementos en el segundo array es de " << lengthAL(array2) << " elementos." << endl; 
    cout << "La capacidad de la union entre el primer y segundo array es de " << lengthAL(array1y2) << " elementos." << endl;
    
    cout << endl;
    cout << "El minimo en el primer array es " << minimo(array1) << "." << endl;
    cout << "El minimo en el segundo array es " << minimo(array2) << "." << endl;
    return 0;
}