int printf(char const*,...);
int main() {
    unsigned long long ch;
    ch = 0x12345678;
    printf(
        "\nPTR %d,%d,0x%llx,%d,%d,%d\n"
        ,100
        ,200
        ,ch
        ,300
        ,400
        ,500
        ,600
        );
    return 0;
}
