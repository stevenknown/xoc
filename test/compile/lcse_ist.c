char * p;

//void foo(int n, char b[], int * cnt)
//{
//    int x,y,z;
//    int k,kb;
//    b[*cnt++] = *(p + n);
//    x = b[k++]; 
//    b[*cnt++] = *(p + n);
//    y = b[k++]; 
//    b[*cnt++] = *(p + n);
//    z = b[k++]; 
//    return x+y+z;
//}

void foo(int n, int * cnt)
{
    int x,y,z;
    int k,kb;
    int a[100];
    a[*cnt++] = 1;
    *(p + n) = (p + n); 
    a[*cnt++] = 2;
    *(p + n) = (p + n); 
    a[*cnt++] = 3;
    *(p + n) = (p + n); 
    return x+y+z;
}

//void bar(int n, int * cnt)
//{
//    int x,y,z;
//    int k,kb;
//    int a[100];
//    a[*cnt++] = (p + n); 
//    *(p + n) = 3;
//}
//
//void zoo(int n, int * cnt)
//{
//    int x,y,z;
//    int k,kb;
//    int a[100];
//    *(p + n) = 3;
//    a[*cnt++] = (p + n); 
//}
