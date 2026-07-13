struct S { int dummy; short e, f; } x,y;
struct S* p = &x;

int foo() {
  x.f=42;
  *p=y;  //**** struct copy
  return x.f;
}

int main() {
    y.f = 1;
    p = &x;
    foo();
    if (1 == x.f) {
        //success
        return 0;
    }
    return 1;
}

