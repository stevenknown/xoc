//Cause IR2OR assert.
struct A {
   int a;
   short mpr[1024][1000];
} ga;

struct B {
   short mpr[1024][1000];
} gb;
void printf(char const*,...);
int main()
{
    if (sizeof(struct B) != sizeof(short)*1024*1000) { return 1; }
    if (sizeof(struct A) != sizeof(short)*1024*1000 + sizeof(int)) { return 2; }
    if (sizeof(struct A) != sizeof(ga)) { return 3; }
    if (sizeof(struct B) != sizeof(gb)) { return 4; }
    return 0;
}
