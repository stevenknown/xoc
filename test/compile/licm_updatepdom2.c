int array (int a[], int size, int start)
{
  int i;
  for (i = 0; i < size; i++)
    if (a[i] != i + start)
      return 1;

  return 0;
}
