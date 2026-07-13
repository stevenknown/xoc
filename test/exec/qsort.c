extern int strcmp(char*, char*);
extern void printf(char*,...);
int hook_cmp(void* a,void* b);
int hook_cmp2(void* a,void* b);
void swap(char *a,char* b,int width);
void qsort(void* base,int nleft,int nright,int width,int (*fhook_cmp)(void*,void*));
typedef struct _Node{
    char *word;
    int cnt;
}Node;

////////////////////////////////
void qsort(void* base,int nleft,int nright,int width,int (*fhook_cmp)(void*,void*))
{
    if (nleft >= nright) return;
    char * lo;
    char * hi;
    lo = (char*)base + width*nleft;
    hi = (char*)base + width*((nleft + nright)/2);
    swap(lo, hi, width);

    int x;
    x = nleft;
    lo = (char*)base + width * nleft;
    int i;
    for(i = nleft + 1 ; i <= nright ; i++){
        hi = (char*)base + width * i;
        if (fhook_cmp(lo, hi) < 0) {
            x++;
            char * ll;
            ll = (char*)base + width * x;
            swap(ll, hi, width);
        }
    }

    hi = (char*)base + width * x;
    swap(lo, hi, width);
    qsort(base, nleft, x - 1, width, fhook_cmp);
    qsort(base, x + 1, nright, width, fhook_cmp);
}


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


void test_qsort2()
{
    Node node[5] = {{"xkey",10}, {"color",3}, {"void",4}, {"static",5}, {"while",2}};
    node[0].word = "xkey";
    node[0].cnt = 10;
    node[1].word = "color";
    node[1].cnt = 3;
    node[2].word = "void";
    node[2].cnt = 4;
    node[3].word = "static";
    node[3].cnt = 5;
    node[4].word = "while";
    node[4].cnt = 2;

    qsort(node, 0, 4, sizeof(node[0]), hook_cmp2);
    int i;
    for(i = 0 ; i < 5 ; i ++){
        printf("%s, %d\n", node[i].word, node[i].cnt);
    }

    printf("\nAfter qsort 2\n");
}


int main()
{
    test_qsort1();
    test_qsort2();
    printf("\nMain returned\n");
    return 0;
}


void swap(char *a,char* b,int width)
{
    char temp;
    while (width --) {
        temp = *a,
        *a++ = *b,
        *b++ = temp;
    }
}


int hook_cmp(void * a,void * b)
{
    return *(int*)a - *(int*)b;
}


int hook_cmp2(void * a,void * b)
{
    Node pa;
    Node pb;
    pa = *(Node*)a;
    pb = *(Node*)b;
    return strcmp(pa.word, pb.word);
}
