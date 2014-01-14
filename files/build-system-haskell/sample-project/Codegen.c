#include <stdio.h>

int main(int argc, char *argv[])
{
    FILE *f;
    f = fopen(argv[1], "w");
    fprintf(f, "#define MY_NAME \"Stefan\"\n");
    return 0;
}
