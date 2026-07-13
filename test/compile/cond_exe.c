int a,b,c,d,x,y;
int w;
void foo()
{
    //a = b > 0 ? c+d, c-d:x+y, x-y;
    a = b > 0 ? c+d:x+y, x-y;
    //c = d > 0 ? a = c+d, b = c-d: w=x+y, w=x-y;
}


void test_cond_exe()
{
    long l;
    char * cl;
    long r;
    l == 4 ? r = cl[3] : 0; //r=cl[3] is effect.
    l >= 3 ? r |= cl[2]<<8 : 0;
    l >= 2 ? r |= cl[1]<<16 : 0;
    r |= cl[0] << 24;
}


struct F {int x,y,z,w;};
struct F simple_select(struct F a, struct F b, struct F c)
{
    struct F result;
    result.x = c.x ? a.x : b.x;
    result.y = c.y ? a.y : b.y;
    result.z = c.z ? a.z : b.z;
    result.w = c.w ? a.w : b.w;
    return result;
}

