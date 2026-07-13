/* search MAJORITY */
int printf(char const*,...);
int n;
/* Input data:
   only use A[1], A[2], ..., A[n].
*/
int A[7 + 1];

int candidate (int m)
{
    int j;
    j = m;
    int c;
    c = A[m];
    int count;
    count = 1;

    while ((j < n) && (count > 0))
    {
        j = j + 1;

        if (A[j] == c)
        {
            count = count + 1;
        }
        else
        {
            count = count - 1;
        }
    }

    if (j == n)
    {
        return c;
    }
    else
    {
        return candidate (j + 1);
    }
}

void display_data (void)
{
    printf ("data:\n");
    int j;
    for (j = 1; j <= n; j++)
    {
        printf ("%d ", A[j]);
    }

    printf ("\nn = %d\n", n);
}

int main (void)
{
    A[0]= 0;
    A[1]= 1;
    A[2]= 3;
    A[3]= 2;
    A[4]= 3;
    A[5]= 3;
    A[6]= 4;
    A[7]= 3;
    n=7;
    int c;
    c = candidate (1);
    int count;
    count = 0;
    int j;
    for (j = 1; j <= n; j++)
    {
        if (A[j] == c)
        {
            count = count + 1;
        }
    }

    display_data ();

    if (count > (n / 2))
    {
        printf ("\ncandidate: %d, count: %d is MAJORITY\n", c, count);
    }
    else
    {
        printf ("\ncandidate: %d, count: %d is not MAJORITY\n", c, count);
    }
    return 0;
}
