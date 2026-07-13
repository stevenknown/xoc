void abort();
int printf(char const*,...);
// tokens and classes (operators last and in precedence order)
enum {
  Num = 128, Fun, Sys, Glo, Loc, Id,
  Char, Else, Enum, If, Int, Return, Sizeof, While,
  Assign, Cond, Lor, Lan, Or, Xor, And, Eq, Ne, Lt, Gt,
  Le, Ge, Shl, Shr, Add, Sub, Mul, Div, Mod, Inc, Dec, Brak
};

int main() {
  int i;
  i = Char;
  printf("\ni=%d\n",i);
  printf("\n%d\n",While);
  printf("\n%d\n",(int)While);
  printf("\n%d,%d,%d,%d,%d,%d,%d,%d\n",
    Char, Else, Enum, If, Int, Return, Sizeof, While);

  while (i <= (int)Brak) {
    printf("\ni=%d\n",i);
    i++;
  } // add keywords to symbol table

  while (i <= Brak) {
    printf("\ni=%d\n",i);
    i++;
  } // add keywords to symbol table

  return 0;
}
