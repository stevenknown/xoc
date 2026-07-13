typedef unsigned long long uint64_t;

static inline uint64_t *prop_hash_end()
{
    return (uint64_t*)0;
}

int main()
{
    int sz = sizeof(prop_hash_end()[0]);
    if (sz != sizeof(unsigned long long)) { return 1; }
    return 0;
}
