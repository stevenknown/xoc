void test_struct_field_ofst()
{
    struct S1 {
        int a;
        struct S1 * next;
        int b;
        int * p;
    } * obj;
    struct S1 s2;
    int t;
    t = s2.b;
    obj->next->b = 0;
    obj->next->p = 0;
}
