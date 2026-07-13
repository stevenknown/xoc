int main()
{
    int * p[2];
    for (int i = 0; i < 10; ++i) {
        p[i] = &i;
    }
    int w = *p[0] - *p[1];
    return w;
}
