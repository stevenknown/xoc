void printf(char const*,...);
int main() {
   float v;
   v = 2;
   printf("\n%d\n", v <= 0.0 ? 100 : 200);

   float bb;
   bb = (v <= 0.0);
   printf("\n%d\n", bb ? 100 : 200);

   return 0;
}
