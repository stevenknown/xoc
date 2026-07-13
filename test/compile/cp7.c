int s715 ()
{
  int rc;
  int i;
  i = 1;
  if (i++, i++, i++, ++i != 6)
      rc = 1;
  return rc;
}
