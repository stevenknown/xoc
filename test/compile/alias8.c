char * p;
int x,y;
int foo()
{
    int i;
    int * l;
    l = &i;
    //Now, the worst may-point-to set contains {MD2,i}
    //Pretend p pointed to {x,y}
    *p = 20;
    return x;
}
