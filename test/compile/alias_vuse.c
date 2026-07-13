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
    dw_fde_node * cie_cfi_head_82;
    dw_fde_node ** p_18;
    dw_cfi_node ** p_27;


    fde_13 = &fde;
    dw_cfi_node * xcfi_22;

    /*
      p_27 = &fde_13->dw_fde_cfi;
      #   VUSE <SMT.48_79>;
      D.1763_23 = fde_13->dw_fde_cfi;
      if (D.1763_23 != 0B) goto <L8>; else goto <L10>;

      # p_18 = PHI <p_30(9), p_27(8)>;
    <L10>:;
      #   cie_cfi_head_82 = V_MAY_DEF <cie_cfi_head_73>;
      *p_18 = xcfi_22;

    GCC bug: The store to *p_18 and the load of fde_13->dw_fde_cfi obviously alias,
    however their virtual operands are disjoint.

    In xoc, *p_18 is alias with fde_13->dw_fde_cfi.
    */

    p_27 = &fde_13->dw_fde_cfi;
    D_1763_23 = fde_13->dw_fde_cfi;
    if (D_1763_23 != 0) {
        //p_18 = &cie_cfi_head_82;
        p_18 = (dw_fde_node **)12;
    } else {
        p_18 = p_27;
    }
    *p_18 = xcfi_22;
    return 0;
}
