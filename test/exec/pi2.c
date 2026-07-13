int printf(char const*,...);
long a=10000,b,c=2800,d,e,f[2801],g;
//Caculate PI to 800 decimal places.
int main()
{
    a=10000;
    c=2800;
    for (;b-c;) {
        f[b++]=a/5;
    }
    for (; d=0, g=c*2; c-=14, printf("%.4d\n",e+d/a), e=d%a)
        for(b=c;d+=f[b]*a,f[b]=d%--g,d/=g--,--b;d*=b);

    return 0;
}
