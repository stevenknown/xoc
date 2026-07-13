int g;
int t1, t2;
void goo(int * p)
{
    g = 0;
    if (t1 > t2) {
        g = 1;
    } else {
        g = 2;
    }
    g = 3;
}


int k()
{
    /*
    struct SS {
        int a;
        int b;
    };
    int z;
    struct SS s;
    s.b=20;
    s=1000; //just to test, it is failed in Cfrontend.
    z=s.b;
    s.a=30;
    z=s.a;
    z=s;
    z=s.b;
    return z;
    */
}


int global;
void f ()
{
  int i;
  i = 1;          // dead store
  global = 1;     // dead store
  global = 2;
  return;
  global = 3;     // unreachable
}


