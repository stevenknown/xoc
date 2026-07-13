//Sample while style loop 
int printf(char const*,...);
int main(void)
{
    int i=0,j=0,sum=0;
    int a=10,b=20;
    int x=0,y=0;
    LAB001:
    i=i+1;
    j=i+j+1;
    if (i<10 && j<30) {
        sum=i+j;
        x=a*b;
        y=a+b;
        goto LAB001;
    }
    i=i+x;
    j=j+y;
    printf("%d,%d\n",i,j);
}
