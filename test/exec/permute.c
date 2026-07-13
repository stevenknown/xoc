/* PERMUTATIONS1 */
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

void perm1 (int m)
{
    if (m == n)
    {
        output ();
    }
    else
    {
        int j;
        for (j = m; j <= n; j++)
        {
            int save;

            /* Swap P[j], P[m]. */
            save = P[j];
            P[j] = P[m];
            P[m] = save;

            perm1 (m + 1);

            /* Swap P[j], P[m]. */
            save = P[j];
            P[j] = P[m];
            P[m] = save;

            /* Now P[m...n] = m, m+1, ..., n. */
        }
    }
}

int main(int argc, char* argv[])
{
    n = 4;
    int j;
    for (j = 1; j <= n; j++)
    {
        P[j] = j;
    }

    perm1 (1);
    return 0;
}
