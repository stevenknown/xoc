int cseaux(int x, int y, int z);
int cse(short b, short a)
{
    //All x+y expression should be removed.
    //Should do GCSE first.
    //And the second is CP.
    int x,y,z;
    a = 0;
    b = 2;
    x = 3;
    y = 4;
    z = x+y;
    if (b) {
        z = b;
        z = x+y;//redundant.
    } else {
        z = a;
        z = x+y;//redundant.
    }
    z = cseaux(x+y, x+y, a+b);//redundant.
    return z;
}



