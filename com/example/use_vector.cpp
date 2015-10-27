#include "stdio.h"
#include "ltype.h"
#include "comf.h"
#include "smempool.h"
#include "sstl.h"
using namespace xcom;
int main()
{
    Vector<char const*> SS;

    SS.set(0, "The number is 10");
    SS.set(1, "The number is 20");
    SS.set(2, "The number is 30");

    printf("Loop by index:\n");

    for(int i = 0; i <= SS.get_last_idx(); i++) {
        printf("%s\n", SS[i]);
    }

    printf("\nReverse Iterator:\n");

    for(int i = SS.get_last_idx(); i >= 0; i--) {
        printf("%s\n", SS[i]);
    }

    printf("\nSample Output:\n");

    printf("%d\n", SS.get_last_idx());
    printf("%s\n", SS[2]);

    return 0;
}
