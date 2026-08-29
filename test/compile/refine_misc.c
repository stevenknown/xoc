int foo(int x, int y)
{
    //x - 0 ==> x
    int a = x - 0;

    //x * 0 ==> 0
    int b = x * 0;

    //x - x ==> 0
    int c = x - x;

    //x + (-y) ==> x-y
    int d = x + (-y);

    //x + (-x) ==> 0
    int e = x + (-x);

    //x & x ==> x
    int f = x & x;

    //x + (~x) ==> -1
    int g = x + (~x);

    return a+b+c+d+e+f+g;
}
