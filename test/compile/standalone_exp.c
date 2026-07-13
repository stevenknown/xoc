enum cplus_tree_code {
AAA=22,
BBB,
};
int foo()
{
    BBB;
    int a;
    a = BBB;
    a; //a is standalone expr.
    return 1;
}
