typedef struct NT {
    int count;
} NT;
typedef struct State {
    char name[128];
    NT nt;
    void *opaque;
} State;

void *foo(State rt) {
    //State ms = rt;
    NT nt = rt.nt;
}
