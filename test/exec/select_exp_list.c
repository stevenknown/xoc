int strcmp(char*, char const*);
static int foo(const char *string, int width, int pad)
{
    if (strcmp(string, "(null)") != 0) { return 3; }
    if (width != 1 || pad != 2) { return 4; }
    return 0;
}
int main()
{
    int width=1,pad=2;
    char * s ="hell";
    char * s2 = "hell";
    s = pad==1 ? "cc":"x","y","z";
    if (strcmp(s, "x") != 0) { return 1; }
    s2 = pad==1 ? "cc":("x","y","z");
    if (strcmp(s2, "z") != 0) { return 2; }
    char * s3 = 0;
    return foo(s3 ? s3 : "(null)", width, pad);
}
