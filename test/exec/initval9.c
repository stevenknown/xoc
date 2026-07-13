union Q
{
  unsigned char n;
  union S2 {
    unsigned short w;
    unsigned long long m;
  } s;
};
void printf(char const*,...);
int main()
{
  union Q q = { 0x00000000000000cc, 0xFFFFffee, };
  if (((unsigned char)q.s.m) != (unsigned char)0xcc) { return 12; }
  return 0;
}
