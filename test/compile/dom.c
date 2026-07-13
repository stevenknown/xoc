int foo(int m)
{
  switch(m)
  {
  case 2:
    for (int i=0; i < 16; i++)
    {
    }
    break;
  case 3:
    for (int i=1; i < 9; i++)
    {
    }
    break;
  default:
    return 1;
  }
  return 0;
}
