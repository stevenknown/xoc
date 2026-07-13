void printf(char const*,...);
int main() {
  int p1;
  int i,j;
  p1 = 1;
  if (i > j) {
    p1 = 2;
  } else {
    p1 = 3;
  }
  if (p1 != 2&&p1!=3) {
    return -1;
  }
  return 0;
}
