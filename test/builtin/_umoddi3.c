unsigned int clz64 (unsigned long long a);
unsigned long long  __umoddi3(unsigned long long a, unsigned long long b)
{
    unsigned int leading_zero_a = clz64(a);
    unsigned int leading_zero_b = clz64(b);
    unsigned int diff_leading_zero = 0;
    unsigned long long quotient = 0;
    int n;

    if (b == 0)
    return 0xffffffffffffffff;

    if (leading_zero_a == leading_zero_b)
    {
        if (a >= b)
            return a - b;
        else // a < b
            return a;

    }
    else if (leading_zero_a > leading_zero_b)
    {
        return a;
    }
    else
    {
        diff_leading_zero = (leading_zero_b - leading_zero_a);
        b = b << diff_leading_zero;

        if (a >= 0x8000000000000000)
        {
            if (a >= b)
            {
                quotient = 1;
                a = a - b;
            }

            b = b >> 1;
            diff_leading_zero--;
        }

        for (n = 0; n < diff_leading_zero + 1; n++)
        {
            quotient = quotient << 1;
            if (a >= b)
            {
                a = a - b;
                quotient++;
            }
            a = a << 1;
        }
    }
    return a >> (diff_leading_zero + 1);
}
