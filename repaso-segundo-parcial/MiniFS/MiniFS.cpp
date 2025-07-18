#include <iostream>
#include "MiniFS.h"
using namespace std;

// #################################################################################################################### \\

FSNode* createNode(string name, bool isFile, int capacidad){
// PROPÓSITO: Crea un nodo con el nombre dado, y si es o no un archivo, con la capacidad de hijos dada.
// COSTO TIEMPO: O(1).
// COSTO ESPACIO: O(1).
    FSNode* node = new FSNode;
    node->nombre = name;
    node->isFile = isFile;
    node->childCount = 0;

    if(!isFile && capacidad > 0){
        node->children = new FSNode*[capacidad];
    } else {
        node->children = NULL;
    }
    
    return node;
}

MiniFS mkFS(string name, int capacidad){
// PROPÓSITO: Crea un nuevo sistema de archivos con una carpeta raíz con el nombre dado y con la capacidad máxima dada.
// COSTO TIEMPO: O(1).
// COSTO ESPACIO: O(1).
    FSStr* fs = new FSStr;
    fs->current = createNode(name, false, capacidad);
    fs->path = emptyS();
    fs->capacidad = capacidad;
    return fs;
}

bool hayNombre(FSNode* node, string name){
// PROPÓSITO: Indica si existe el nombre dado en el nodo dado.
// COSTO TIEMPO: O(N).
// COSTO ESPACIO: O(1).
    int i = 0;

    while(i < node->childCount && node->children[i]->nombre != name){
        i++;
    }

    return node->childCount > i;
}


void mkDir(string name, MiniFS fs){
// PROPÓSITO: Agrega un nuevo directorio como hijo del nodo actual, si hay espacio y no hay otro archivo o directorio con el  
//			  mismo nombre. Caso contrario, no hace nada.
// COSTO TIEMPO: O(N).
// COSTO ESPACIO: O(1).
    if(fs->current->childCount < fs->capacidad && !fs->current->isFile && !hayNombre(fs->current, name)){
        fs->current->children[fs->current->childCount++] = createNode(name, false, fs->capacidad);
    }
}

void touch(string name, MiniFS fs){
// PROPÓSITO: Agrega un nuevo archivo como hijo del nodo actual, si hay espacio y no hay otro archivo o directorio con el mismo nombre. 
//			  Caso contrario, no hace nada.
// COSTO TIEMPO: O(N).
// COSTO ESPACIO: O(1).
    if(fs->current->childCount < fs->capacidad && !fs->current->isFile && !hayNombre(fs->current, name)){
        fs->current->children[fs->current->childCount++] = createNode(name, true, 0);
    }
}

void cd(int index, MiniFS fs){
// PROPÓSITO: Se mueve al hijo index del directorio actual. Si el nodo actual no es un directorio o no existe un nodo en dicho index,
//            no hace nada. 
// COSTO TIEMPO: O(1).
// COSTO ESPACIO: O(1).
    if(!fs->current->isFile && index >= 0 && index < fs->current->childCount){
        push(fs->current, index, fs->path);
        fs->current = fs->current->children[index];
    }
}

void cdUp(MiniFS fs){
// PROPÓSITO: Sube un nivel en el sistema. Si está en la raíz no hace nada.
// COSTO TIEMPO: O(1).
// COSTO ESPACIO: O(1).
    if(!isEmptyS(fs->path)){
        fs->current = top(fs->path);
        pop(fs->path);
    }
}

void print(string content){
// PROPÓSITO: Imprime el string dado en la consola.
// COSTO TIEMPO: O(1).
// COSTO ESPACIO: O(1).
    cout << content << endl;
}

void ls(MiniFS fs){
// PROPÓSITO: Imprime los nombres de los hijos del nodo actual, si el nodo actual es un directorio. No hace nada si es un archivo.
// COSTO TIEMPO: O(N).
// COSTO ESPACIO: O(1).
    for(int i = 0; i < fs->current->childCount; i++){
        print(fs->current->children[i]->nombre);
    }
}

void rm(int index, MiniFS fs){
// PROPÓSITO: Elimina el archivo en la posición dada (si existe) en el directorio actual, y mueve los demás hijos para no dejar espacio 
//			  entre sí. En caso de no existir, no hace nada.
// COSTO TIEMPO: O(N).
// COSTO ESPACIO: O(1).
    if(index >= 0 && index < fs->current->childCount && fs->current->children[index]->isFile){
        delete fs->current->children[index];

        for(int i = index; i < fs->current->childCount; i++){
            fs->current->children[i] = fs->current->children[i++];
        }

        fs->current->childCount--;
    }
}

int childCount(MiniFS fs){
// PROPÓSITO: Devuelve la cantidad de archivos y directorios del nodo actual.
// COSTO TIEMPO: O(1).
// COSTO ESPACIO: O(1).
    return fs->current->childCount;
}

string currentName(MiniFS fs){
// PROPÓSITO: Devuelve el nombre del archivo/directorio actual.
// COSTO TIEMPO: O(1).
// COSTO ESPACIO: O(1).
    return fs->current->nombre;
}

bool isFile(MiniFS fs){
// PROPÓSITO: Indica si el nodo actual es un archivo.
// COSTO TIEMPO: O(1).
// COSTO ESPACIO: O(1).
    return fs->current->isFile;
}

// #################################################################################################################### \\