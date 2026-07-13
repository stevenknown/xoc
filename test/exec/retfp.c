void abort();
float f()
{
    return -1.5f;
}
double g()
{
    return -2.5f;
}
int main(void)
{
    if (f() != -1.5f) {
        abort();
    }
    if (g() != -2.5f) {
        abort();
    }
    return 0;
}
