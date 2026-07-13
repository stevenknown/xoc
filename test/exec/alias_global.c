int printf(char const*,...);
struct S { int dummy; short e, f; } x,y;
struct S* p;

void foobar() {
  x.f = 42;
}

int main() {
    y.f = 1;
    x.f = 2;
    foobar();
    if (42 == x.f) {
        printf("\nsuccess\n");
        return 0;
    }
    return 1;
}

