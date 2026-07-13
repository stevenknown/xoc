void abort();
void printf(char const*,...);
int fun1(short a)
{
    return a;
}

int fun2(long long a)
{
    return a;
}

long long fun2_2(long long a)
{
    return a;
}

float fun3(long a)
{
    return a;
}

int fun4(float a)
{
    return a;
}

double fun5(int a)
{
    return a;
}

float fun6(double a)
{
    return a;
}

double fun7(float a)
{
    printf("\n%f\n", (double)a);
    return a;
}

long long fun8(short a)
{
    return a;
}

void assert(char v) { if (v==0) abort(); }

int main()
{
    assert(fun1(65536)==0);
    assert(fun2(0x1ffffFFFF)==0xffffFFFF);
    assert(fun2_2(0x1ffffFFFFll)==0x1ffffFFFFll);
    assert(fun3(0x1)==1.00f);
    assert(fun4(1.0000f)==0x1);
    assert(fun5(0x1)==1.000000);
    assert(fun6(0.0001)==0.0001f);

    //1 should be converted to double(1).
    assert(fun6((double)1)==1.0f);

    printf("\n%f,%f\n", (double)0.0001f, 0.0001);
    assert(fun7(0.0001f)!=0.0001);
    assert(fun7(1.0f)==1);

    //65535 should be converted to short(-1).
    assert(fun8(65535)==-1);
    return 0;
}
