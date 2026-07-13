typedef struct A A;
typedef struct BT B;
typedef struct CX {
    int a;
} CX;

A * var;
A * bar(void);
struct A * bar2(void);
struct CX * bar3(void);
CX * bar3(void);
void foo()
{
    A * v1;
    struct A * v2;
    B * v3;
    struct BT * v4;
    CX v5;
    struct CX v6;
}
