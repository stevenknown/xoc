int main()
{
    char const* tt;
    tt = "ab\0c" "\0def";
    tt = "abc" "dce";
    tt = "\1234567890ABCDEF";
    tt = "\765";
    tt = "\0xef";
    tt = "\xAB";
    tt = "\XAB";
    return 0;
}
