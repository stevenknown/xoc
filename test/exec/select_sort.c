void printf(char const*,...);
int n;

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
    if (i < n)
    {
        int k;

        k = i;
        int j;
        for (j = i + 1; j <= n; j++)
        {
            if (A[j] < A[k])
            {
                k = j;
            }
        }

        if (k != i)
        {
            /* Swap A[i], A[k] */
            int save;
            save = A[i];
            A[i] = A[k];
            A[k] = save;
        }

        sort (i + 1);
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

    sort (1);

    display_data ();

    return 0;
}
