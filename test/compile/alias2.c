int main(int i)
{
    //verify flow insensitive point to.
    char d[100];
    int a,b,*p, c;
    for (;;) {
        if (i) {
            p = &a;
            i += d[c];
            *p = 2; //p->a
        } else if (i > 12) {
            p = &b;
            *p = 3 - d[i] + a; //p->b
        } else {
            p = &d[i]; //p->d
        }
        *p = 10; //p->{a,b,d}
        *p = 20; //p->{a,b,d}
    }
}
