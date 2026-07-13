int main()
{
    int pad=2;
    char * s ="hell";
    s = pad==1 ? "cc":"x","y","z";
    if (s[0] == 'x') { return 0; }
    return 1;
}
