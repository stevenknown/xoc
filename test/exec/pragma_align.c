struct S { int dummy; short e, f; char g[13]; int h;} x,y;
int main() {
    struct S a; //a is aligned in target default alignment.
#pragma align(8)
    struct S b; //b is aligned in 8 bytes.
#pragma align(16)
    struct S c; //c is aligned in 16 bytes.

    if (((int)&a) % 4 != 0) {
        //stack variable can not align.
        return 1;
    }
    if (((int)&b) % 4 != 0) {
        //stack variable can not align.
        return 2;
    }
    if (((int)&c) % 4 != 0) {
        //stack variable can not align.
        return 3;
    }
    return 0;
}
