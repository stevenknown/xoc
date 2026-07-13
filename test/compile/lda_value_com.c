struct foo {char x, y, z[2];};
struct foo p, q;
struct foo *m, *n;
struct foo u[5], v[5];
int m,n;
void bar(int baz)
{
  p = q;
  p.z[baz] = 1;
  bar(m=n);
  p.z[baz] = 2;
  (p = q).z[baz] = 1;
  (*m = *n).z[baz] = 1;
  (m[1] = n[2]).z[baz] = 1;
  (u[1] = v[2]).z[baz] = 1;
}
