#include <iostream>
#include "Stack.h"
using namespace std;

// #################################################################################################################### \\

Stack apilar(int length, int* array){
// PROPÓSITO: Describe un Stack con los elementos del array dado sin alterar el orden de los elementos.
// COSTO TIEMPO: O(length) Amortizado.
// COSTO ESPACIO: O(1).
    Stack s = emptyS();

    for(int i = 0; i < length; i++){
        push(array[i], s);
    }

    return s;
}

int* desapilar(Stack s){
// PROPÓSITO: Describe un array en base a un Stack dado sin alterar el orden de los elementos.
// COSTO TIEMPO: O(size).
// COSTO ESPACIO: O(1).
    int sizeS = size(s);
    int* array = new int[sizeS];

    for(int i = sizeS; i > 0; i--){
        array[i - 1] = top(s);
        pop(s);
    }

    destroyS(s);

    return array;
}

void insertarEnPosicion(Stack s, int x, int posicion){
// PROPÓSITO: Dada una posicion válida en la stack y un elemento, ubica dicho elemento en dicha posición 
//            (se desapilan elementos hasta dicha posición y se inserta en ese lugar).
// COSTO TIEMPO: O(N) Peor caso.
// COSTO ESPACIO: O(1).
    if(posicion >= 0 && posicion < size(s)){
        // En caso de necesitar insertar en el medio del rango actual del Stack, se inserta en dicha posición.
        
        Stack auxS = emptyS(); 
        int sizeS = size(s);

        // Se desapila hasta la posición dada, guardando cada valor en un Stack auxiliar.
        for(int i = sizeS - 1; i >= posicion; i--){
            push(top(s), auxS);
            pop(s);
        }

        // Una vez desapilado todo hasta la posición dada, se coloca el elemento dado.
        push(x, s);

        // Se vuelven a recolocar todos los los elementos que se quitaron anteriormente y se guardaron en el Stack auxiliar.
        while(!isEmptyS(auxS)){
            push(top(auxS), s);
            pop(auxS);
        } 

        // Una vez terminado, el Stack auxiliar debería estar vacío y ya sin un uso necesario.
        destroyS(auxS);
    
    } else {
        // En caso que esté fuera de rango, se inserta a lo último.
        push(x, s);
    }
}


void PrintStack(Stack s){
// PROPÓSITO: Imprime todos los elementos en el Stack.
// COSTO TIEMPO: O(N).
// COSTO ESPACIO: O(1).
    Stack auxS = emptyS();

    // Quito lo que haya en el Stack original para copiarlo en un Stack auxiliar (quedan en orden inverso).
    while(!isEmptyS(s)){
        push(top(s), auxS);
        pop(s);
    }

    // Ahora copio lo que esta ahi en un Stack que resulta la copia del original y en el original en si.
    if(!isEmptyS(auxS)){
        int topS = top(auxS);
        pop(auxS);
        cout << topS;
        push(topS, s);
    }

    while(!isEmptyS(auxS)){
        int topS = top(auxS);
        pop(auxS);
        cout << ", " << topS;
        push(topS, s);
    }

    // Siendo que ya esta copiado en ambos Stacks resultantes, se borra el Stack auxiliar.
    destroyS(auxS);
}

// #################################################################################################################### \\

/* INTERFAZ STACK:
    * Stack emptyS();
    * bool isEmptyS(Stack s);
    * void push(int x, Stack s);
    * int top(Stack s);
    * void pop(Stack s);
    * int size(Stack s);
    * void destroyS(Stack s);
*/

// #################################################################################################################### \\

int main(){
    Stack primerStack = emptyS();
    Stack segundoStack = emptyS();
    cout << boolalpha << "El primer Stack esta vacio:  " << isEmptyS(primerStack) << "." << endl;
    cout << boolalpha << "El segundo Stack esta vacio: " << isEmptyS(segundoStack) << "." << endl;
    cout << endl;
    cout << "Despues de haber agregado algunos elementos..." << endl;
    cout << endl;
    push(99, primerStack);
    push(77, primerStack);
    push(52, primerStack);
    push(33, primerStack);
    push(17, primerStack);
    push(1, segundoStack);
    push(3, segundoStack);
    push(5, segundoStack);
    push(7, segundoStack);
    cout << boolalpha << "El primer Stack esta vacio:  " << isEmptyS(primerStack) << "." << endl;
    cout << boolalpha << "El segundo Stack esta vacio: " << isEmptyS(segundoStack) << "." << endl;
    cout << endl;
    cout << "El primer Stack tiene " << size(primerStack) << " elementos." << endl;
    cout << "El segundo Stack tiene " << size(segundoStack) << " elementos." << endl;
    cout << endl;
    cout << "Todos los elementos en el primer Stack son: "; PrintStack(primerStack); cout << "." << endl;
    cout << "Todos los elementos en el segundo Stack son: "; PrintStack(segundoStack); cout << "." << endl;
    cout << endl;
    cout << "Despues de haber quitado un par de elementos..." << endl;
    cout << endl;
    pop(primerStack);
    pop(primerStack);
    pop(primerStack);
    pop(segundoStack);
    cout << "El primer Stack tiene " << size(primerStack) << " elementos." << endl;
    cout << "El segundo Stack tiene " << size(segundoStack) << " elementos." << endl;
    cout << endl;
    cout << "Todos los elementos en el primer Stack son: "; PrintStack(primerStack); cout << "." << endl;
    cout << "Todos los elementos en el segundo Stack son: "; PrintStack(segundoStack); cout << "." << endl;
    cout << endl;
    cout << "Despues de haber elimnado el segundo Stack..." << endl;
    cout << endl;
    destroyS(segundoStack);
    cout << "El primer Stack tiene " << size(primerStack) << " elementos." << endl;
    cout << "El segundo Stack tiene " << size(segundoStack) << " elementos. Lo cual no tiene sentido porque fue borrada." << endl;
    cout << endl;
    cout << "Todos los elementos en el primer Stack son: "; PrintStack(primerStack); cout << "." << endl;
    cout << "Todos los elementos en el segundo Stack son: "; PrintStack(segundoStack); cout << "." << endl;
    return 0;
}

// #################################################################################################################### \\