struct S {
  long long la[20];
} s[10];
int main()
{
    s[1].la[3] = 20;
    s[2].la[4] = ++s[1].la[3];
    if (s[2].la[4] != 21) { return 1; }
    if (s[1].la[3] != 21) { return 2; }
    return 0;
}
