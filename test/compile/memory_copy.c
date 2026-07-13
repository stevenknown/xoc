struct S {
    char buf[256];
};

struct S s1;
int main(struct S * p, struct S * q)
{
    s1 = *p;
    *q = s1;
    return 0; 
}
