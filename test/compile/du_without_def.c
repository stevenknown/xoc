typedef struct _tag {
  int a;
}dw_cfi_node;

typedef struct dw_fde_struct
{
  const char *dw_fde_current_label;
  dw_cfi_node *dw_fde_cfi;
} dw_fde_node;

int alias_vuse_test()
{
    dw_fde_node fde;
    dw_fde_node * fde_13;
    dw_fde_node * D_1763_23;
    dw_fde_node ** p_18;
    fde_13 = &fde;
    D_1763_23 = fde_13->dw_fde_cfi;
    if (D_1763_23 != 0) {
        p_18 = (dw_fde_node **)12;
    }
    return 0;
}
