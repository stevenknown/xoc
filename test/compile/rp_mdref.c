void qsort(void* base,int nleft,int nright,int width,int (*fhook_cmp)(void*,void*));
int hook_cmp(void* a,void* b);
extern void printf(char*,...);
void test_qsort1()
{
    int a[10] = {1,2,3,4,5,6,7,8,9,10};
    qsort(a, 0, 9, sizeof(int), hook_cmp);
    int i;
    for(i = 0; i < 10 ; i++) {
      printf("%d ", a[i]);
    }

    printf("\nAfter qsort 1\n");
}


