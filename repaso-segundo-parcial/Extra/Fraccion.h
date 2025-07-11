#include <iostream>
using namespace std;

// #################################################################################################################### \\

struct FraccionStr{
// INVARIANTE DE REPRESENTACIÓN:
//  * Para que exista la fracción, el denominador debe ser diferente de cero.
    int numerador;   // Representa el numerador de la fracción.
    int denominador; // Representa el denominador de la fracción.
};

typedef FraccionStr* Fraccion;

// #################################################################################################################### \\

Fraccion fraccion(int x, int y);
// PROPÓSITO: Describe una fracción con el númerador y el denominador dado.
// COSTO TIEMPO: O(1).
// COSTO ESPACIO: O(1).

Fraccion sumar(Fraccion f1, Fraccion f2);
// PROPÓSITO: Describe la suma de las dos fracciones dadas sin simplificarlas.
// COSTO TIEMPO: O(1).
// COSTO ESPACIO: O(1).

Fraccion restar(Fraccion f1, Fraccion f2);
// PROPÓSITO: Describe la resta de las dos fracciones dadas sin simplificarlas.
// COSTO TIEMPO: O(1).
// COSTO ESPACIO: O(1).

Fraccion multiplicar(Fraccion f1, Fraccion f2);
// PROPÓSITO: Describe la multiplicación de las dos fracciones dadas sin simplificarlas.
// COSTO TIEMPO: O(1).
// COSTO ESPACIO: O(1).

Fraccion dividir(Fraccion f1, Fraccion f2);
// PROPÓSITO: Describe la división de las dos fracciones dadas sin simplificarlas.
// COSTO TIEMPO: O(1).
// COSTO ESPACIO: O(1).

void invertir(Fraccion f1);
// PROPÓSITO: Invierte la fracción dada sin simplificarla.
// COSTO TIEMPO: O(1).
// COSTO ESPACIO: O(1).

void simplificar(Fraccion f1);
// PROPÓSITO: Simplifica la fracción dada.
// COSTO TIEMPO: O(log N), siendo N = min(numerador, denominador).
// COSTO ESPACIO: O(1).

void destruir(Fraccion f1);
// PROPÓSITO: Destruye la fracción dada.
// COSTO TIEMPO: O(log N), siendo N = min(numerador, denominador).
// COSTO ESPACIO: O(1).

// #################################################################################################################### \\