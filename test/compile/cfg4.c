float a;
int test_cfg_opt(char * p)
{
ENTRY: 
  goto FOR_BODY;  //this BB is not trampoline, because the label can not
                  //be merged into target bb.
FOR_BODY:
  a = *p;     
  if (a > 0) {
    goto FOR_BODY;
  }
  return 0;
}
