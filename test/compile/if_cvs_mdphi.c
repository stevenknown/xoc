static int numgp;
static int numfp;
typedef struct {
    int len;
    void* ty;
} Vec;
int len(Vec*vec);
Vec *get(Vec*vec, int i);
bool is_f(void*ty);

static void set(Vec *args) {
    numgp = 0;
    numfp = 0;
    for (int i = 0; i < len(args); i++) {
        Vec *a = get(args, i);
        if (is_f(a->ty))
            numfp++;
        else
            numgp++;
    }
}
