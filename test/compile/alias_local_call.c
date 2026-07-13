void foo(char*);
void main(int i)
{
    int a;
    int b;
    char * p = (char*)&a;
    char * q;
    a = 10;
    if (i > 0) {
       p[3] = 20; //S1:mustdef MD15,maydef MD6(local_may_alias)
    }

    //NOTE: MD15 did not reach here.
    foo(q);
}
