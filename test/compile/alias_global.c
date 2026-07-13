typedef struct dw_cfi_struct
{
  struct dw_cfi_struct *dw_cfi_next;
  const char *dw_cfi_addr;
}
dw_cfi_node;

typedef struct dw_fde_struct
{
  const char *dw_fde_current_label;
  dw_cfi_node *dw_fde_cfi;
}
dw_fde_node;

unsigned fde_table_in_use;
dw_fde_node * fde_table;

void * strdup(const char*);
const char* dwarf2out_cfi_label ();
dw_cfi_node * new_cfi();

void add_fde_cfi ( const char* label, dw_cfi_node * cfi)
{
  dw_fde_node * fde;
  fde = fde_table + fde_table_in_use - 1;

  if (*label == 0) {
      label = dwarf2out_cfi_label ();
  }

  if (fde->dw_fde_current_label == ((void*)0)) {
       fde->dw_fde_current_label = label = strdup (label);

    dw_cfi_node * xcfi;
       xcfi = new_cfi();

       xcfi->dw_cfi_addr = label;
    void * u;
    u = xcfi->dw_cfi_addr;
  }
}
