int foo()
{
    int i;
    int j = 3;
    for (i = 1; i <= 123; i+=5) {
        j = i + 7 + j;
    }
    return j;
} 
