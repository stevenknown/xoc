typedef struct _RangeCoder
{
    unsigned char one_state[256];
} RangeCoder;
static inline void put_rac(RangeCoder *c, unsigned char* const state)
{
  *state= c->one_state[*state];
}
typedef struct _PlaneContext{
    unsigned (*state)[32];
} PlaneContext;
static inline void put_symbol(RangeCoder *c, unsigned char *state)
{
    int i;
    const int e;
    put_rac(c, state);
    for(i=e-1; i>=0; i--)
      put_rac(c, state+22+i);
}
int put_symbol(RangeCoder * const, unsigned b[32]);
int encode_line(void)
{
    PlaneContext * const p;
    RangeCoder * const c;
    int a;
    put_symbol(c, p->state[a]);
}
