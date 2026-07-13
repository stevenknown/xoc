
void * malloc(int);
void alias_1_func()
{
  int *arr;
  arr = (int*) malloc(sizeof(int)*10);
  int i;
  for(i=0;i<10;i++)
    arr[i] = i;

  int *b;
  b = &arr[5]; //Note b points to the heap memory.

  int **c;
  c = &arr; //But c point to arr.

  int **d;
  d = &arr + 4; //d also point to arr.
}
