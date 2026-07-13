int a;
int a2;
int a3;
int b;
int g;
int z;
int * gp;
int * gp2;

/*
void foo()
{
    if (b > 0) {
        a = 1;
    } else {
        a = 2;
    }
    g = a; //use, no domdef
}

void foo2()
{
    a2 = 20;
    if (b > 0) {
        a2 = 1;
    } else {
        a2 = 2;
    }
    g = a2; //use, no domdef
}

void foo3()
{
    if (b > 0) {
        a3 = 1;
    } else {
        a3 = 2;
    }
    a3 = 20; //S1
    int l;
    if (b > 1) {
        l = 3;
    }
    g = *gp; //use, S1 is domdef
}

void foo4()
{
    for (int i = g; i < 10; i++) { 
      if (b > 0) {
          a3 = 1;
      } else {
          a3 = 2;
      }
      a3 = 20; //S1
      int l;
      if (b > 1) {
          l = 3;
      }
      g = *gp; //use, S1 is domdef
      a3 = 30;
    }
}

void foo5()
{
    for (int i = g; i < 10; i++) { 
      g = *gp; //use, no domdef
      a3 = 30;
    }
}
*/

/*
void foo6()
{
    for (int i = g; i < 10; i++) { 
      if (b > 0) {
          a3 = 1;
      } else {
          a3 = 2;
      }
      a3 = 20; //S1
      int l;
      if (b > 1) {
          l = 3;
      }
      g = *gp; //use, S1 is domdef
      if (l > 4) {
          l = 5;
      }
      a3 = 30;
    }
}

void foo7()
{
    for (int i = g; i < 10; i++) { 
        if (b > 0) {
            a3 = 1;
        } else {
            a3 = 2;
        }
        a3 = 20; //S1
        int l;
        if (b > 1) {
            l = 3;
        }
       
        int j = 0;
        do {
            g = *gp; //use, S1 is domdef
            if (l > 4) {
                l = 5;
            }
            a3 = 30;
            j++;
        } while (j < 10);
    }
}
*/

void foo8()
{
    a = 1;
    a = 2; //S1
    a = 3;
    return *gp; //use, domdef is S1
}


