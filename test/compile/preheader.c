int g;
int n;
int z;
int main(int b)
{
  goto LOOP_HEAD;

 LOOP_START: 
   n = 10;
LOOP_HEAD:
  //There is no need to insert guard BB.
  g = b; //only LD b can be hoisted.
  if (b > 0) { goto LOOP_START; }
  return g;
}
