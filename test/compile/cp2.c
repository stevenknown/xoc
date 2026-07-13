struct S {
    char buf[256];
};

int main(struct S * p, struct S * q)
{
    struct S s1;
    s1 = *p; //*p should be propagated.
    *q = s1;
    return 0; 
}
