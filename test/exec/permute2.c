// a5_8.cpp : Defines the entry point for the console application.
//
/* PERMUTATIONS2 */

int n = 4;
int P[4 + 1];
void printf(char const*,...);

void output (void)
{
    int j;
    for (j = 1; j <= n; j++)
    {
        printf ("%d ", P[j]);
    }
    printf ("\n");
}

void perm2 (int m)
{
    if (m == 0)
    {
        output ();
    }
    else
    {
        int j;
        for (j = 1; j <= n; j++)
        {
            if (P[j] == 0)
            {
                P[j] = m;
                perm2 (m - 1);
                P[j] = 0;
            }
        }
    }
}

int main(int argc, char* argv[])
{
    n = 4;
    int j;
    for (j = 1; j <= n; j++)
    {
        P[j] = 0;
    }
    perm2 (n);
    return 0;
}
