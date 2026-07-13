/* Bug in reorg.c, deleting the "++" in the last loop in main.
   Origin: <hp@axis.com>.  */
extern void *malloc(int);
extern void abort();
extern void exit(int);
extern int printf(char const*,...);
int strcmp(const char *s1, const char *s2);

extern void f (void);
extern int x (int, char **);
extern int r (const char *);
extern char *s (char *, char **);
extern char *m (char *);
char *u;
char *h;
int check = 0;
int o = 0;
static char c[2] = "b";
static int cnt = 0;

int main (int argc, char **argv)
{
  check = 0;
  o = 0;
  c[0]='b'; c[1]=0;
  cnt = 0;
  char *args[] = {"a", "b", "c", "d", "e"};
  args[0] = "a";
  args[1] = "b";
  args[2] = "c";
  args[3] = "d";
  args[4] = "e";

  if (x (5, args) != 0 || check != 2 || o != 5)
    abort ();
  exit (0);
}

int x (int argc, char **argv)
{
  int opt = 0;
  char *g = 0;
  char *p = 0;
  opt = 0;
  g = 0;
  p = 0;

  if (argc > o && argc > 2 && argv[o])
    {
      g = s (argv[o], &p);
      if (g)
    {
      *g++ = '\0';
      h = s (g, &p);
      if (g == p)
        h = m (g);
    }
      u = s (argv[o], &p);
      if (argv[o] == p)
    u = m (argv[o]);
    }
  else
    abort ();

  while (++o < argc)
    if (r (argv[o]) == 0)
      return 1;

  return 0;
}

char *m (char *x) { abort (); }
char *s (char *v, char **pp)
{
  if (strcmp (v, "a") != 0 || check++ > 1)
    abort ();
  *pp = v+1;
  return 0;
}

int r (const char *f)
{
//static char c[2] = "b";
//static int cnt = 0;

  if (*f != *c || f[1] != c[1] || cnt > 3)
    abort ();
  c[0]++;
  cnt++;
  return 1;
}
