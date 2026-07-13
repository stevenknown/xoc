int strcmp(char const* a, char const* b);
int main (int argc, char **argv)
{
  char * local[] = {"a", "b", "c", "d", "e"};
  char local2[10][20] = { {1,2}, {3,4,5} };
  char local3[10][20][30] = {
                              { //dim0
                                { //dim1
                                  1, //dim2
                                  2},
                                {3,4,5},
                                {6,7,},
                              },
                              {
                                {8,9,10,},
                                {11,},
                                {12,13,},
                              },
                            };
  if (strcmp(local[0], "a") != 0) { return 1; }
  if (strcmp(local[1], "b") != 0) { return 1; }
  if (strcmp(local[2], "c") != 0) { return 1; }
  if (strcmp(local[3], "d") != 0) { return 1; }
  if (strcmp(local[4], "e") != 0) { return 1; }

  if (local2[0][0] != 1) { return 2; }
  if (local2[0][1] != 2) { return 2; }
  if (local2[1][0] != 3) { return 2; }
  if (local2[1][1] != 4) { return 2; }
  if (local2[1][2] != 5) { return 2; }

  if (local3[0][0][0] != 1) { return 3; }
  if (local3[0][0][1] != 2) { return 3; }
  if (local3[0][1][0] != 3) { return 3; }
  if (local3[0][1][1] != 4) { return 3; }
  if (local3[0][1][2] != 5) { return 3; }
  if (local3[0][2][0] != 6) { return 3; }
  if (local3[0][2][1] != 7) { return 3; }
  if (local3[1][0][0] != 8) { return 3; }
  if (local3[1][0][1] != 9) { return 3; }
  if (local3[1][0][2] != 10) { return 3; }
  if (local3[1][1][0] != 11) { return 3; }
  if (local3[1][2][0] != 12) { return 3; }
  if (local3[1][2][1] != 13) { return 3; }

  return 0;
}
