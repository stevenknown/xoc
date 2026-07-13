int a,b,c;
int d;
void exit(int);
void init();
int main()
{
    init(); 
    //1. c = (a & 0xFF) & 0xF ==> c = a & 0xF
    //2. c = (0xFF & a) & 0xF ==> c = a & 0xF
    //3. c = 0xF & (a & 0xFF) ==> c = a & 0xF
    //4. c = 0xF & (0xFF & a) ==> c = a & 0xF
    d = a & 0xFF;
    c = 0xf & d;
    if (c!=0x6) { exit(1); }

    c = (0xFF & a) & 0xF;
    if (c!=0x6) { exit(2); }

    c = 0xF & (a & 0xFF);
    if (c!=0x6) { exit(3); }

    c = 0xF & (0xFF & a);
    if (c!=0x6) { exit(4); }

    return 0;
}
void init()
{
    a = 0x123456;
}
