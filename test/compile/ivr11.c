int foo(int *a, int n, int c)
{
    int hare , tortoise, step;
    for (hare = 1, tortoise = 3, step = 5; hare < tortoise;
         hare+=4, tortoise+=2, step+=1)
    {;}
    return step;
}
