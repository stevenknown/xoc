typedef struct { } empty_t;

void bar (empty_t);
void f ()
{
  empty_t i;
  bar (i);
}
