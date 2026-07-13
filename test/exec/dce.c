void printf(char const*,...);
struct {
  int a;
  int b;
} gs, gy;
int main ()
{
  //Test processing to local NON-killing DEF.
  gs.a = 10;  //S1, should be killed by S3
  gs.a = 20;  //S3
  gs.b = 30;  //S4
  gy = gs;    //S5, should use S4,S3,S2
  if (gy.a != 20 && gy.b == 30) {
     printf("\nfailed!\n");
  }
  return 0;
}
