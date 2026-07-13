#pragma hint 
int foo()
{
    int x;
    int i; #pragma __nothing__ !#%%&*^&( [][afsdf][ a ];'adgva'
    #pragma hmpp <grp_label> [codelet_label]? directive_type [,directive_parameters]* [&]
    int * p;
    int * q;
    p = &x;
    q = &x;
    for (i = 0; i < 10; i++) {
        *(p+2) = x + *q;
    }
}

