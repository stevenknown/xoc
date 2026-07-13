struct S {
  int a,b;
} s[10];
int foo(int i, struct S * p)
{
    int a;
    s[i].b = 20; //S1  
    a = s[i].b + 3; //S2  
    //We know at least S1 and S2 are accessing same location.

    p[i].b = 40; //S3  
    a = a + p[i].b; //S4  
    //We know at least S3 and S4 are accessing same location.
    
    return a; //After CP, 'a' is 63. return a; ===> return 63;
}
