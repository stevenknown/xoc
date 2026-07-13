typedef struct _S {
  int j[11];
  int i;
}S;
int * p;
int foo()
{
    S s; 
    int a;
    for (s.i = 0; s.i < 100; s.i++) { //i should be IV.
        *p++;
    }
    return a;
}
