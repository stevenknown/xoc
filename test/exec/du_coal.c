float gf[100];
int printf(char const*,...);
void foo();
void foo()
{
    float f00, f10, f36;
    f36=20;
    float *pf;
    int i;
    for (i = 0; i < 2; i++)
    {
        pf = &gf[0];
        f00 = *(pf++);
        f10 = *pf;
        pf = gf;
        *(pf++) = f00;
        *pf = f10;
        *pf = f36;
    }
}

int main()
{
    gf[0]=2;
    gf[1]=4;
    gf[2]=8;
    gf[3]=16;
    foo();
    //cvt(f32,i32) must be inserted.
    if (gf[0]!=2||
        gf[1]!=20||
        gf[2]!=8||
        gf[3]!=16){
        return 1;
    }
    return 0;
}
