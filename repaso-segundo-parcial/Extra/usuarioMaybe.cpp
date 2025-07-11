#include <iostream>
#include "Maybe.h"
using namespace std;

// #################################################################################################################### \\

/* INTERFAZ MAYBE:
    * MaybeInt nothing();
    * MaybeInt just(int x);
    * int fromJust(MaybeInt m);
    * bool isNothing(MaybeInt m);
*/

// #################################################################################################################### \\

int main(){
    MaybeInt primerMabye = nothing();
    MaybeInt segundoMaybe = just(22);
    cout << boolalpha << "El primer maybe es nothing:  " << isNothing(primerMabye) << "." << endl;
    cout << boolalpha << "El segundo maybe es nothing: " << isNothing(segundoMaybe) << "." << endl;
    cout << endl;
    cout << "El valor del unico maybe que se le puede obtener valor es: " << fromJust(segundoMaybe) << "." << endl;
    return 0;
}