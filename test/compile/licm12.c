int g[100];
void foo(int a, int b)
{
    for (int i = 0; i < 10; i++)
      for (int j = 0; j < 10; j++)
        for (int k = 0; k < 10; k++)
          for (int m = 0; m < 10; m++)
            g[i+j+k+m] = a + b; //a+b should be moved to outest loop.
}
