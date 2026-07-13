typedef int Value;
typedef struct VarRef {
    union {
        int header;
        int mid;
        struct {
            int var_base;
            short var_idx;
        };
        int tail;
    };
    Value value;
} VarRef;

int main()
{
    VarRef x;
    x.header = 33;
    x.var_idx = 14;
    if (x.var_base != 33) { return 1; }
    if (x.tail != 33) { return 2; }

    x.mid = 27;
    if (x.var_idx != 14) { return 3; }
    return 0;
}
