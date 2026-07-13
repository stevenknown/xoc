struct A {
    float a1;
    int a2;
    float a3;
};

struct B {
    double b1;
    struct A b2;
    struct A * b3;
};

int main()
{
    struct A a;
    a.a3 = 0.01f; //S1

    struct B b;
    b.b3 = &a; //S2

    float * n = &b.b3->a3; //n can not be initialized before S2
    if (*n != 0.01f) { return 2; }

    return 0;
}
