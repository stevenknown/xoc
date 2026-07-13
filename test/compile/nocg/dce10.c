void printf(char const*,...);
struct {
  int a;
  int b;
} gs, gy;
int main ()
{
  //Test processing to local NON-killing DEF.
  gs.a = 10;  //S1, should be killed by S3
  gs = 0;     //S2, note the stmt can not be CodeGen.
  gs.a = 20;  //S3
  gs.b = 30;  //S4
  gy = gs;    //S5, should use S4,S3,S2
  if (gy.a != 20) {
     printf("\nfailed!\n");
  }
  return 0;
}
