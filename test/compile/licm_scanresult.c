int test_16bit_lsr()
{
    int err = 0;
    unsigned int results[200];
    unsigned int x[100];
    unsigned int z;
    unsigned int i, j;
    for (i = 0; i < 100; i++) {
        for (j = 0; j < 200; j++) {
            if (j != x[i]) {
                err++;
            }
        }
    }
    return err;
}
