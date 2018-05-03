void fun(int, int);

#include <stdio.h>
#include <stdlib.h>

int main()
{
    fun(10, 20);
    puts("!!!Hello World!!!");
    return 0;
}

void fun(int a, int b)
{
    int i = 0;

    while(a < b)
    {
        ++i;
        ++a;
        printf("%i\n", i);
    }
}
