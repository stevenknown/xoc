int g;
int i;
int test_insert_preheader()
{
    g = 11;
    goto HEAD;
LOOP:
    g = 12;
HEAD:
    if (i > 0) {
        goto END;
    }
    g = 13; 
END:
    return 0;
}
