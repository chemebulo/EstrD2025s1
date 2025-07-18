#include <iostream>
#include "MiniFS.h"
using namespace std;

// #################################################################################################################### \\

int countFiles(MiniFS fs){
// PROPÓSITO: Devuelve la cantidad de archivos que hay en el MiniFS dado.
    int count = 0;

    if(isFile(fs)){
        count ++;
    } else {

        for(int i = 0; i < childCount(fs); i++){
            cd(i, fs);
            count += countFiles(fs);
            cdUp(fs);
        }
    }

    return count;
}

void cdPath(MiniFS fs, string* path, int len){
// PROPÓSITO: Realiza toda la ruta dada en el MiniFS dado.
// PRECONDICIÓN: La ruta es válida a partir de la posición actual, por lo tanto, se que la posición actual es un directorio.
    for(int i = 0; i < len; i++){
        string next = path[i];
        int j = 0;
        cd(j, fs);

        while(currentName(fs) != next){
            cdUp(fs);
            cd(++j, fs);
        }
    }
}

// #################################################################################################################### \\

/* INTERFAZ MINIFS:
    * MiniFS mkFS(string name, int capacidad);
    * void mkDir(string name, MiniFS fs);
    * void touch(string name, MiniFS fs);
    * void cd(int index, MiniFS fs);
    * void cdUp(MiniFS fs);
    * void ls(MiniFS fs);
    * void rm(int index, MiniFS fs);
    * int childCount(MiniFS fs);
    * string currentName(MiniFS fs);
    * bool isFile(MiniFS fs);
*/ 

// #################################################################################################################### \\

int main(){
    return 0;
}

// #################################################################################################################### \\