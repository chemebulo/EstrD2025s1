#include <stdio.h>
#include <string.h>

#ifdef _WIN32
    #include <windows.h>
#else
    #include <unistd.h>
#endif

void delay_ms(int ms) {
#ifdef _WIN32
    Sleep(ms);
#else
    usleep(ms * 1000);
#endif
}

void imprimirConDelay(const char *texto, int msPorLetra, int msEntreFrases) {
    for (size_t i = 0; i < strlen(texto); i++) {
        putchar(texto[i]);
        fflush(stdout);
        delay_ms(msPorLetra);
    }
    delay_ms(msEntreFrases);
}

int main() {
    printf("\n");
    imprimirConDelay("Dangerous feelings break out my soul \n", 70, 650);
    imprimirConDelay("It's just the meaning of being alone \n", 70, 650);
    imprimirConDelay("I need you here, wherever you are \n", 70, 800);
    imprimirConDelay("I need you now to take me so far... \n\n", 70, 600);

    imprimirConDelay("I wanna run like the speed of the sound \n", 65, 650);
    imprimirConDelay("I was somewhere, I feel you around \n", 65, 550);
    imprimirConDelay("You give me now the meaning of life \n\n", 70, 2000);

    imprimirConDelay("With you, I'm feeling alive... \n\n", 65, 500);
    imprimirConDelay("Oooh... \n", 90, 2800);
    imprimirConDelay("Oooh... \n", 90, 950);
    imprimirConDelay("Oooh... \n", 90, 850);
    imprimirConDelay("Oooh... \n", 90, 2800);
    imprimirConDelay("Oooh... \n", 90, 950);
    imprimirConDelay("Oooh... \n\n", 90, 1700);

    imprimirConDelay("Why you're lookin' like that? \n", 60, 500);
    imprimirConDelay("I'm burning like fire \n", 60, 500);
    imprimirConDelay("I wanna be higher \n", 60, 500);
    imprimirConDelay("Just let me know \n", 60, 200);
    imprimirConDelay("Why you're lookin' like that? \n", 60, 500);
    imprimirConDelay("You're driving me crazy \n", 60, 350);
    imprimirConDelay("You're lookin' amazing \n\n", 60, 600);
    imprimirConDelay("Oooh... \n\n", 90, 950);
    return 0;
}