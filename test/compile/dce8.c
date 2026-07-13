void foo(char ** w)
{
    *w = 900;  //change main's i.
}

int g;
int main()
{
    char arr[100];
    int i,j;
    i = 800;
    char * p;
    char * q;
    q = &i;
    p = arr;
    arr[3] = 100; //should be removed.
    if (p == 0) {
        arr[1] = 10; //should be removed.
        arr[3] = 400; //should be removed, but DCE do not seem able to do it.
                      //Guess, DSE may be the better candidate pass.
        g = 500;
    } else {
        arr[2] = 20;
    }
    arr[3] = 10;
    foo(&q); //q is not changed, but i changed
    return arr[3] + arr[2] + g;
}
