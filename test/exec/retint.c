void abort();
long long gv;
long long h()
{
    return -gv;
}
long f()
{
    return -15;
}
long long g()
{
    return -25;
}

int main(void)
{
    gv = 5;
    if (f() != -15) {
        abort();
    }
    if (g() != -25) {
        abort();
    }
    if (h() != -5) {
        abort();
    }
    return 0;
}
