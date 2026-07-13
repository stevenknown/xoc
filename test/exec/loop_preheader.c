extern void exit(int);
extern void abort();
void printf(char const*,...);
int main()
{
    int a,b;
    a = 1;
    goto LOOP_HEAD;
    while (1) {
        b = a + 100;
        LOOP_HEAD:
            if (a > 0) {
                break;
            }
    }
    printf("\nsuccess\n");
    return 0;
}
