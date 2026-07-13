#include "../include/io.h"
#ifdef DE
#define LEN 100
#else
#define LEN 1000
#endif
int aa[LEN];
void foo(char i)
{
  aa[i++] = 0;
  printf("%u", aa[i]);
}
