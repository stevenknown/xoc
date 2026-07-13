typedef struct base_obj base_obj;
typedef struct type_obj type_obj;
struct base_obj
{
  struct type_obj *ob_type;
  int ob_refcnt;
};
struct type_obj
{
  base_obj tp_base;
  void (*tp_dealloc) (base_obj *);
};
extern void type_del (base_obj *);
type_obj type_type = {
  { &type_type, 1},
  type_del
};
