static int g_AAA[] = {
    1, 2, 3, 4
};

static int g_BBB[] = {
    1, 2, 3, 4
};

int var_replace_mod()
{
    if (g_AAA[2] == 3) {
        return 1;
    }
    g_AAA[3] = g_BBB[2];
    return 0;
}

int output(int x[]) { return 1; }
int sum;
int main (void)
{
    //If g_AAA is declared as static, and there is no any strange behaviors
    //such as (*(char*)0x100)=20 , then g_AAA[0] should be replaced to 1.
    sum = g_AAA[0];
    if (sum > 0 && var_replace_mod()) {
        output(g_AAA);
    }
    return 0;
}

