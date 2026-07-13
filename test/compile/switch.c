int foo(int a)
{
    switch (a) {
    case 10: return 1;
    case 20: return 2;    break;
    default: return 20;
    case 3: return 3;
    }
    return a;
}
