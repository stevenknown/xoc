int main()
{
    int cm_s = 10, *cm = &cm_s;
    if (cm_s != 10) { return 1; }
    if (cm != &cm_s) { return 2; }
    return 0;
}
