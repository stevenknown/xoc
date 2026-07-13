int a,b,c,d,e,g;
int main()
{
    a=1;
    b=2;
    c=3;
    d=4;
    e=7;
    g=5;
    if (a==1 && --b==1 && c++==3 && (e=c)==3 && d++==5) { // d++ should not execute.
        g=3;
    }
    if (b!=1) {
        return 1;
    }
    if (c!=4) {
        return 2;
    }
    if (d!=4) {
        return 3;
    }
    if (e != 4) {
        return 4;
    }
    if (g!=5) {
        return 5;
    }
    return 0;
}
