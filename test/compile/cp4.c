extern int printf(const char *, ...);
int s85()
{
  int diff[7], j;
  static char *type[] = {
    "float",
    "double"
  };
  struct
  {
    int twobit:2;
    int threebit:3;
  } s3;
  for (j = 0; j < 5; j++)
    printf("%s%d", type[j], diff[j]);
  s3.threebit = 7;
  s3.twobit = s3.threebit;
  s3.threebit = s3.twobit;
  return 0;
}
