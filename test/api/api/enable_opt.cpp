#include "header_for_xgen.h"
#include "enable_opt.h"

void enableO3()
{
  xoc::g_opt_level = OPT_LEVEL3;
  xoc::g_do_cp = true;
  xoc::g_do_cp_aggressive = true;
  xoc::g_do_dce = true;
  xoc::g_do_dce_aggressive = true;
  xoc::g_do_licm = true;
  xoc::g_do_rce = true;
  xoc::g_do_vect = true;
  xoc::g_do_rp = true;
  xoc::g_do_lftr = true;
  xoc::g_do_prssa = true;
  xoc::g_do_mdssa = true;
  xoc::g_infer_type = true;
  // CFG opt
  xoc::g_do_cfg_opt = true;
  xoc::g_do_cfg_remove_empty_bb = true;
  xoc::g_do_cfg_remove_unreach_bb = true;
  xoc::g_do_cfg_remove_trampolin_bb = true;
  xoc::g_do_invert_brtgt = true;
  xoc::g_do_cfg_remove_redundant_branch = true;
  xoc::g_do_cfg_remove_trampolin_branch = true;
  xoc::g_do_cfg_remove_redundant_label = true;
}


