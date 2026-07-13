int main()
{
    int e1 = sizeof(
       "ab\0c" "\0def"
    );
    if (e1 != 9) {
        return -1;
    }
    return 0;
}
