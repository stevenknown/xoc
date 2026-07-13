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
    a.a3 = 0.01f;

    struct B b;
    b.b2.a2 = 3;
    b.b3 = &a;

    int * m = &b.b2.a2;
    if (*m != 3) { return 1; }

    float * n = &b.b3->a3;
    if (*n != 0.01f) { return 2; }

    int o = b.b2.a2;
    if (o != 3) { return 3; }

    float p = b.b3->a3;
    if (p != 0.01f) { return 4; }

    struct B * q = &b;
    struct A * u = &*(q->b3);
    struct A * v = *&(q->b3);
    if (u->a3 != 0.01f) { return 5; }
    if (v->a3 != 0.01f) { return 6; }

    return 0;
}
