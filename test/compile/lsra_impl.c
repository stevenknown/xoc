int lsra_impl_bug()
{
  int j;
  int svtest();
  int zero ();
  for (j = 0; j < 3; j++)
    if (svtest() != zero()) {
	    j = 1;
    }
  return j;
}
