int main()
{
    typedef float ty[3];
    ty *tt;
    if (sizeof(ty) != 12) {
        return 1;
    }
    if (sizeof(tt) != sizeof(char*)) {
        return 2;
    }
    return 0;
}
