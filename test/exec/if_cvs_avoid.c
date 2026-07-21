int a,d;
void foo(int i)
{
    //The IF can NOT be conversioned.
    if (i > 0) {
        a = d;
        d = 10;
    } else {
        d = 20;
        a = d;
    }
}

int main()
{
    d = 1;
    a = 0;
    foo(1); 
    if (a != 1) { return 1; }
    if (d != 10) { return 2; }

    d = 1;
    a = 0;
    foo(0); 
    if (a != 20) { return 3; }
    if (d != 20) { return 4; }
    return 0;
}
