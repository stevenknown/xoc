int printf(char const*, ...);
int strcmp(const char*, const char*);

int main(int argc, char ** argv)
{
  char a[10] = {1,2};
  char (*pa)[10] = &a;
  if ((*pa)[1] != a[1]) { return 1; }

  char b[10][20] = {{1,2}, {2,3}};
  char (*pb)[10][20] = &b;
  if ((*pb)[1][1] != b[1][1]) { return 2; }

  char * c[2];
  c[0] = "hello";
  c[1] = "world";
  char *(*pc)[2] = c;
  char **pcc = c;
  if ((*pc)[0][1] != c[0][1]) { return 4; }
  if (*((*pc)[1]) != c[1][0]) { return 5; }
  if (***pc != c[0][0]) { return 6; }
  if (**pcc != c[0][0]) { return 7; }
  if (strcmp((*pc)[1], c[1]) != 0) { return 8; }
  if (*((*pc)[1] + 1) != c[1][1]) { return 9; }
  if ((*pcc)[1] != c[0][1]) { return 10; }
  if (strcmp(*(*pc), c[0]) != 0) { return 11; }
  if (**(*pc) != c[0][0]) { return 12; }
  if ((*(*pc))[2] != c[0][2]) { return 13; }

  return 0;
}
