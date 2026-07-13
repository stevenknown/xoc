extern double atof (const char *__nptr);

void bar (char *s)
{
  union {double val; unsigned int a, b;} u;
  u.val = atof (s);
}
