struct Par {
    int x;
    int y;
};

Par consPar(int n, int m);
// PROPÓSITO: Construye un par.

int fst(Par p);
// PROPÓSITO: Devuelve la primera componente.

int snd(Par p);
// PROPÓSITO: Devuelve la segunda componente.

int maxDelPar(Par p);
// PROPÓSITO: Devuelve la mayor componente.

Par swap(Par p);
// PROPÓSITO: Devuelve un par con las componentes intercambiadas.

Par divisionYResto(int n, int m);
// PROPÓSITO: Devuelve un par donde la primer componente es la división y la segunda el resto entre ambos números.