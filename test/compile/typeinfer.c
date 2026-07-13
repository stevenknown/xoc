
typedef struct tagBase {
    int x,y;
} Base;

typedef struct tagPoint3d {
    int x,y,z;
} Point3d;

void * malloc(int a);
void test_typeinfer()
{
    Point3d *p, **x;
    p = (Point3d*)malloc(sizeof(Point3d));
    x = &p;
    //...
    *x = (Base*)malloc(sizeof(Base)); //change the type of p.
}
