typedef struct _Node{
    char *word;
    int cnt;
}Node;
extern void printf(char*,...);
int hook_cmp2(void* a,void* b);
void qsort(void* base,int nleft,int nright,int width,int (*fhook_cmp)(void*,void*));
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
}
