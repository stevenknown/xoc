void printf(char const*,...);
void foo(int i, char const* str)
{
    printf("%s = %d", str, i);
}

int main()
{
    foo(100, "hellovariable");
    return 0;
}
