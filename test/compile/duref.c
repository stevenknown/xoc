//Distinguish s.a and s.b[i], they are not overlapped.
struct S {
    int a;
    int b[10];
};
void foo(int i)
{
    struct S s1;
    s1.b[i] = 20;
    s1.a = 10;
    //Although s1.b[i] is unbound, its offset is not zero.
    //so s1.b[i] should not overlap to s.a
    return s1.b[i];
}
