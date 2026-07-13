int foo(int aa[], int bb[])
{
    //aa, bb is actually a pointer.
    int i = 5;
    do {
        bb[i] = aa[i]; //should not be removed by DCE.
        i--;
    } while (i >= 0);
    return 0;
}


