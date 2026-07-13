int n;
void printf(char const*,...);
int A[9+1] =
{
    0,
    3, 1, 4, 1, 5, 9, 2, 6, 5
};

void display_data (void)
{
    printf ("data:\n");
    int j;
    for (j = 1; j <= n; j++)
    {
        printf ("%d ", A[j]);
    }
    printf ("\n");
}

void sort (int i)
{
    if (i > 1)
    {
        int x, j;

        x = A[i];
        sort (i - 1);

        j = i - 1;
        while (j > 0 && A[j] > x)
        {
            A[j + 1] = A[j];
            j = j - 1;
        }

        A[j + 1] = x;
    }
}


int main(int argc, char* argv[])
{
    n = 9;
    A[0] = 0;
    A[1] = 3;
    A[2] = 1;
    A[3] = 4;
    A[4] = 1;
    A[5] = 5;
    A[6] = 9;
    A[7] = 2;
    A[8] = 6;
    A[9] = 5;

    display_data ();

    sort (n);

    display_data ();

    return 0;
}
