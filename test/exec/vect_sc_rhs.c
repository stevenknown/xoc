struct S {
    char buf[256];
};
struct S s1, s2;
int main()
{
    for (int i = 0; i < 100; i++) {
        s2.buf[i] = 100 - i; //can NOT vectorized because RHS is scalar.
    }
    return 0; 
}
