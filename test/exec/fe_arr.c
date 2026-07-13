int printf(char const*, ...);
int open(const char * pathname, int flags);

int main(int argc, char ** argv)
{
  int fd;
  char * ppp[3];
  ppp[0] = "c4.out";
  ppp[1] = "-s";
  ppp[2] = "c4.input";
  argv = ppp;
  argc = 3;
  printf("\n%d, %d, %d\n", argv[0][0], (*argv)[0], **argv);
  --argc; ++argv;
  if (argc > 0)
    if (**argv == '-')
      if ((*argv)[1] == (int)'s')
      {
         --argc;
         ++argv;
      }

  printf("\nargc:%d, *argv:%s\n", argc, *argv);
  if ((fd = open(*argv, 0)) < 0) {
    printf("could not open(%s)\n", *argv);
    return -1;
  }
  return 0;
}
