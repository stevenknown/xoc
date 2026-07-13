typedef int Value;
typedef struct VarRef {
    union {
        int header;
        struct {
            int var_base;
            short var_idx;
        };
    };
    Value value;
} VarRef;

int foo()
{
    VarRef x;
    x.header = 0;
    x.var_idx = 1;
}
