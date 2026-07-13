extern void printf(char*,...);
int hook_cmp(void* a,void* b);
void swap(char*a, char*b);
void qsort(void* base,int nleft,int nright,int (*fhook_cmp)(void*,void*));

int main()
{
  int a[10];
    a[0] = 1;
    a[1] = 2;
    a[2] = 3;
  //a[3] = 4;
  //a[4] = 5;
  //a[5] = 6;
  //a[6] = 7;
  //a[7] = 8;
  //a[8] = 9;
  //a[9] = 10;
    qsort(a, 0, 2, hook_cmp);
    int i;
    for(i = 0; i < 3; i++) {
      printf("%d ", a[i]);
    }

    printf("\n");
    return 0;
}


void foo() {}
void swap(char*a, char*b)
{
    char temp;
    int w;
    w = 4;
    while (w--) {
        temp = *a;
        *a = *b;
        a++;
        *b = temp;
        b++;
    }
}

void qsort(void* base,
           int nleft,
           int nright,
           int (*fhook_cmp)(void*,void*))
{
    printf("\n>> %d, %d\n", nleft, nright);
    if (nleft >= nright) return;
    char * lo;
    char * hi;

    lo = ((char*)base) + 4*nleft;
    hi = ((char*)base) + 4*((nleft + nright)/2);
    printf("\n<<<< %d,%d,%d,%d", (int)*lo, (int)*(lo+1), (int)*(lo+2), (int)*(lo+3));
    printf("\n<<<< %d,%d,%d,%d", (int)*hi, (int)*(hi+1), (int)*(hi+2), (int)*(hi+3));
    swap(lo, hi);
    printf("\n>>>> %d,%d,%d,%d", (int)*lo, (int)*(lo+1), (int)*(lo+2), (int)*(lo+3));
    printf("\n>>>> %d,%d,%d,%d", (int)*hi, (int)*(hi+1), (int)*(hi+2), (int)*(hi+3));

    int x;
    x = nleft;
    lo = ((char*)base) + 4 * nleft;
    int i;
    for(i = nleft + 1 ; i <= nright ; i++){
        hi = ((char*)base) + 4 * i;
        int y;
        y = (*(int*)lo) - (*(int*)hi);
        printf("\n>>> %d, %d, %d, %d", i, y, (*(int*)lo), (*(int*)hi));
        if (y < 0) {
        //if (fhook_cmp(lo, hi) < 0) {
            x++;
            char * ll;
            ll = ((char*)base) + 4 * x;
            swap(ll, hi);
        }
    }

  //hi = ((char*)base) + 4 * x;
  //swap(lo, hi);
  //qsort(base, nleft, x - 1, fhook_cmp);
  //qsort(base, x + 1, nright, fhook_cmp);
}



int hook_cmp(void * a,void * b)
{
    return *(int*)a - *(int*)b;
}
