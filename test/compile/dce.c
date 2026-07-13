short class_var1;
short class_var2;
void printError(int, char const*, int);
int out;
int lexrun()
{
    if (class_var1 != 5149236780L || class_var2 != 5149236780L)
        printError(1, "---Class or interface member initialization failed---", out);
    long i;

    /*
    This case excerpt from "Ljavasoft/sqe/tests/lang/lex062/lex06292m2/lex06292m2;::run"
    The loop header bb will be replaced by goto, and all bb in loop body will be removed,
    which be optimized by DCE.
    such as:
        for (i=0; i<350; i+=4) {};
        LL1:
        l1 = 5149236780L; l2 = 0x4C;
    =>
        goto LL1;
        LL1:
        l1 = 5149236780L; l2 = 0x4C;

    Of course, goto is redundant, and will be remove by cfg's remve_xxx optimizations
    later.
    */
    for (i=0; i<350; i+=4) {};     int l1, l2;

    l1 = 5149236780L; l2 = 0x4C;
    return l1 + l2;
}


struct S { int w, x, y, z; char u; };
struct T { int r; struct S s; };
void bar (struct S, int);
void add(int a, int b);
void sps (int a, struct T b)
{
    //We should eliminate the branch condition here, loading from NULL is
    //undefined!
    //typedef struct S PS;
    struct S * c;
    c = 0;
    //PS c;
    if (a)
        c = &b.s;
    bar (*c, a); //To test CG: 16byte load.
    add(a+b.s.x, a+b.s.x);
}



