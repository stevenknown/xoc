extern void printf(char*,...);
int hook_cmp(void* a,void* b);
void swap(char*a, char*b);
void qsort(void* base,int nleft,int nright,int (*fhook_cmp)(void*,void*));
void test_qsort1();


int main()
//void test_qsort1()
{
  int a[10];
    a[0] = 5;
    a[1] = 2;
    a[2] = 3;
    a[3] = 4;
    a[4] = 1;
    a[5] = 9;
    a[6] = 7;
    a[7] = 8;
    a[8] = 6;
    a[9] = 10;


  qsort(a, 0, 9, hook_cmp);
  char * lo;
  lo = (char*)a;
  char * hi;
  hi = (char*)a;
  //printf("\n%x,%x\n", (int)lo, (int)hi);
  swap(lo,hi);

  int i;
  for(i = 0; i < 9; i++) {
    printf("%d ", a[i]);
  }

  //printf("\nAfter qsort1\n");
  return 0;
}


void swap(char*a, char*b)
{
    int temp;
//
////int * a2;
////a2 = a;
////int * b2;
////b2 = b;
////temp = *a,
////*a = *b,
////*b = temp;

    int w;
    w = 4;
    while (w--) {
        temp = *a;
        *a++ = *b;
        *b++ = temp;
    }
}

void qsort(void* base,
           int nleft,
           int nright,
           int (*fhook_cmp)(void*,void*))
{
    if (nleft >= nright) return;
    char * lo;
    char * hi;
    lo = (char*)base + 4*nleft;
    hi = (char*)base + 4*((nleft + nright)/2);
    swap(lo, hi);
    return;

  //int x;
  //x = nleft;
  //lo = (char*)base + 4 * nleft;
  //int i;
  //for(i = nleft + 1 ; i <= nright ; i++){
  //    hi = (char*)base + 4 * i;

  //    if ((*(int*)lo - *(int*)hi) < 0) {
  //    //if (fhook_cmp(lo, hi) < 0) {
  //        x++;
  //        char * ll;
  //        ll = (char*)base + 4 * x;
  //        swap(ll, hi);
  //    }
  //}
  //
  //hi = (char*)base + 4 * x;
  //swap(lo, hi);
  //qsort(base, nleft, x - 1, fhook_cmp);
  //qsort(base, x + 1, nright, fhook_cmp);
}



int hook_cmp(void * a,void * b)
{
    return *(int*)a - *(int*)b;
}

