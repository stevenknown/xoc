int g;
int main()
{
    int i=0;
    int * p = &g;
    for (;i<5;i++) {
        *p = 20; //IR_CP will propagate and change *p to g.
                 //Have to CHECK the MDSSA DU
    }
    return !(g == 20);
}
