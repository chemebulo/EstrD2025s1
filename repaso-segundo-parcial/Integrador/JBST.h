#include <iostream>
using namespace std;

struct JBSTNodeStr{
    string name;         // Nombre de la persona.
    JBSTNodeStr* left;   // Empleados con nombre menor a name.
    JBSTNodeStr* right;  // Empleados con nombre mayor a name.
    JBSTNodeStr* parent; // Superior jerárquico directo de name.   
};

struct JBSTStr{
    JBSTNodeStr* root;
};

typedef JBSTStr* JerarquiaBST;

// #################################################################################################################### \\

JerarquiaBST fundar(string p);
// PROPÓSITO: Crea la jerarquía, colocando a la persona p como presidente.
// COSTO TIEMPO: O(1).
// COSTO ESPACIO: O(1).

void insertar(string nuevo, string superior, JerarquiaBST t);
// PROPÓSITO: Modifica la jerarquía t agregando a la persona de nombre nuevo como subordinado directo
//            de la persona cuyo nombre es superior.
// PRECONDICIONES: 
//   * Existe una persona en la jerarquía con el nombre del superior dado.
//   * No existe una persona en la jararquía con el nombre nuevo.
// COSTO TIEMPO: O(N).
// COSTO ESPACIO: O(1).

bool esSubordinadoDe(string empleado, string superior, JerarquiaBST bst);
// PROPÓSITO: Indica si la persona con el nombre empleado es subordinada (directa o indirectamente) de la
//            persona con el nombre superior.
// PRECONDICIONES:
//   * Existe una persona en la jararquía con el nombre del superior dado.
//   * Existe una persona en la jerarquía con el nombre del empleado dado.
// COSTO TIEMPO: O(N).
// COSTO ESPACIO: O(1).