#include <iostream>
#include "StackFS.h"
using namespace std;

// #################################################################################################################### \\

struct FSStr{
// INVARIANTES DE REPRESENTACIÓN:
//	* MiniFS no es NULL.
//	* current no es NULL, es un puntero a un nodo válido.
//	* capacidad tiene que ser mayor a cero.
//	* Si path tiene elementos:
//		* Es un Stack en el cual cada nodo es hijo del que está debajo.
//		* El nodo actual es hijo del tope del Stack.
//		* La base del Stack es el nodo raíz.
//	* Si path está vacío, current es la raíz del filesystem.
	FSNode* current;  // Representa el nodo actual que tiene el MiniFS.
	Stack path;		  // Representa la ruta del MiniFS.
	int capacidad;	  // Representa la capacidad de hijos máximos de un directorio en el MiniFS.
};

typedef FSStr* MiniFS;

// #################################################################################################################### \\

FSNode* createNode(string name, bool isFile, int capacidad);
// PROPÓSITO: Crea un nodo con el nombre dado, y si es o no un archivo, con la capacidad de hijos dada.
// COSTO TIEMPO: O(1).
// COSTO ESPACIO: O(1).

MiniFS mkFS(string name, int capacidad);
// PROPÓSITO: Crea un nuevo sistema de archivos con una carpeta raíz con el nombre dado y con la capacidad máxima dada.
// COSTO TIEMPO: O(1).
// COSTO ESPACIO: O(1).

void mkDir(string name, MiniFS fs);
// PROPÓSITO: Agrega un nuevo directorio como hijo del nodo actual, si hay espacio y no hay otro archivo o directorio con el  
//			  mismo nombre. Caso contrario, no hace nada.
// COSTO TIEMPO: O(N).
// COSTO ESPACIO: O(1).

void touch(string name, MiniFS fs);
// PROPÓSITO: Agrega un nuevo archivo como hijo del nodo actual, si hay espacio y no hay otro archivo o directorio con el mismo nombre. 
//			  Caso contrario, no hace nada.
// COSTO TIEMPO: O(N).
// COSTO ESPACIO: O(1).

void cd(int index, MiniFS fs);
// PROPÓSITO: Se mueve al hijo index del directorio actual. Si el nodo actual no es un directorio o no existe un nodo en dicho index,
//            no hace nada. 
// COSTO TIEMPO: O(1).
// COSTO ESPACIO: O(1).

void cdUp(MiniFS fs);
// PROPÓSITO: Sube un nivel en el sistema. Si está en la raíz no hace nada.
// COSTO TIEMPO: O(1).
// COSTO ESPACIO: O(1).

void ls(MiniFS fs);
// PROPÓSITO: Imprime los nombres de los hijos del nodo actual, si el nodo actual es un directorio. No hace nada si es un archivo.
// COSTO TIEMPO: O(N).
// COSTO ESPACIO: O(1).

void rm(int index, MiniFS fs);
// PROPÓSITO: Elimina el archivo en la posición dada (si existe) en el directorio actual, y mueve los demás hijos para no dejar espacio 
//			  entre sí. En caso de no existir, no hace nada.
// COSTO TIEMPO: O(N).
// COSTO ESPACIO: O(1).

int childCount(MiniFS fs);
// PROPÓSITO: Devuelve la cantidad de archivos y directorios del nodo actual.
// COSTO TIEMPO: O(1).
// COSTO ESPACIO: O(1).

string currentName(MiniFS fs);
// PROPÓSITO: Devuelve el nombre del archivo/directorio actual.
// COSTO TIEMPO: O(1).
// COSTO ESPACIO: O(1).

bool isFile(MiniFS fs);
// PROPÓSITO: Indica si el nodo actual es un archivo.
// COSTO TIEMPO: O(1).
// COSTO ESPACIO: O(1).

// #################################################################################################################### \\