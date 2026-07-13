extern int printf(char const*,...);
typedef union tree_node * tree;
enum tree_code {
  A
};
struct tree_common
{
  union tree_node *chain;
  union tree_node *type;
  enum tree_code code : 8;
  unsigned s1 : 1;
  unsigned s2 : 29;
  unsigned s3 : 2;
  unsigned s4 : 1;
};
union tree_node
{
  struct tree_common common;
};
int main()
{
    tree convs;
    //printf("\n%d\n", sizeof (struct tree_common));
    //TODO:support aggrating bit-field into integer type.
    struct tree_common s;
    if (((char*)&s.type - (char*)&s.chain) != sizeof(union tree_node*)) {
        return 1;
    }
    if (((char*)&s.code - (char*)&s.type) != sizeof(union tree_node*)) {
        return 2;
    }
    if (((char*)&s.s1 - (char*)&s.code) != sizeof(enum tree_code)) {
        return 3;
    }
    if (((char*)&s.s2 - (char*)&s.s1) != sizeof(unsigned)) { return 4; }
    if (((char*)&s.s3 - (char*)&s.s2) != sizeof(unsigned)) { return 5; }
    if (((char*)&s.s4 - (char*)&s.s3) != sizeof(unsigned)) { return 6; }
    //unsigned s1 = s.s1;
    //unsigned s2 = s.s2;
    //unsigned s3 = s.s3;
    //unsigned s4 = s.s4;
    return 0;
}
