#include <stdio.h>
#include "Hello.h"

void say_hello(const char *name)
{
    fprintf(stdout, "Hello %s\n", name);
}
