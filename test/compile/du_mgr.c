int foo()
{
    int z;
    typedef struct {
        int a;
        int b;
    } S;
    S s;
    s.b = 20; //override s.
    //s = 1000; //s can override s.a and s.b.
    z = s.b; //also use s
    s.a = 30; //s.a can not overide s.b and s.
    z = s.a; //also use s
    z = s; //also use s.a, s.b
    z = s.b; //also use s
    return z;
}
