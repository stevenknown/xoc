struct foo {char x, y, z[2];};
struct foo p, q;
struct foo *m, *n;
struct foo u[5], v[5];

void bar(int baz)
{
  (u[1] = v[2]).z[baz] = 1;
}

void bar2(int baz)
{
  (m[1] = n[2]).z[baz] = 1;
}

void bar3(int baz)
{
  (p = q).z[baz] = 1;
}

int main()
{
    int baz = 1;

    bar(baz);
    if (u[1].z[baz] != 1) { return 1; }

    m=&u;
    n=&v;
    bar2(baz);
    if (m[1].z[baz] != 1) { return 2; }

    bar3(baz);
    if (p.z[baz] != 1) { return 3; }

    return 0;
}
