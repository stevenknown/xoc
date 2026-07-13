int foo()
{
    int i;
    int j = 3;
    for (i = 1; i <= 123;) {
        i = i + 4; //i has multiple def, not biv.
        j = i + 7 + j;
        i = i + 5;
    }
    return j;
}
