OPT_OBJS+=\
cfs_opt.o\
dbg.o\
goto_opt.o\
if_opt.o \
du.o\
ir.o\
ir_bb.o\
ir_builder.o\
ir_du.o\
ir_cfg.o\
ir_simp.o\
ir_gvn.o\
ir_rce.o\
ir_dce.o\
ir_cp.o\
ir_lcse.o\
ir_gcse.o\
ir_licm.o\
ir_middle_opt.o\
ir_high_opt.o\
ir_expr_tab.o\
cdg.o\
ir_refine.o\
ir_rp.o\
ir_aa.o\
ir_ssa.o\
ir_mdssa.o\
mdssainfo.o\
label.o\
data_type.o \
option.o\
region.o\
region_mgr.o\
util.o\
var.o\
md.o\
loop.o\
cfs_mgr.o\
pass_mgr.o\
inliner.o\
ipa.o\
callg.o\
ir_loop_cvt.o\
refine_duchain.o\
prssainfo.o\
scalar_opt.o\
liveness_mgr.o\
mdliveness_mgr.o\
du_helper.o\
symtab.o\
logmgr.o\
ai.o\
gscc.o\
lftr.o\
ir_ivr.o\
md_mgr.o\
solve_set.o\
infer_type.o\
attachinfo_mgr.o\
analysis_instr.o\
invert_brtgt.o\
ir_helper.o\
gr_helper.o\
loop_dep_ana.o\
mdssalive_mgr.o\
insert_guard_helper.o\
optctx.o\
ir_dump.o\
ir_dump_ext.o\
ir_mgr.o\
ir_mgr_ext.o\
ir_verify.o\
ir_verify_ext.o\
ir_decl.o\
ir_desc.o\
ir_utils.o\
multi_res_convert.o\
cfg_lifting.o\
revise_cfg.o\
bytebuf.o\
linear_rep.o\
storage_space.o\
literal.o\
chain_recur.o\
insert_cvt.o\
act_mgr.o\
dwarf.o

ifeq ($(TARG),)
  $(error "NEED TARG INFO:e.g:make TARG=FOR_DEX TARG_DIR=../dex")
endif

ifeq ($(TARG_DIR),)
  $(error "NEED TARG_DIR INFO:e.g:make TARG=FOR_DEX TARG_DIR=../dex")
endif

ifeq ($(FOR_IP), true)
  $(info "CONDBR:opt/Makefile.inc:FOR_IP=$(FOR_IP)")
  OPT_OBJS+=\
  lsra_impl.o\
  lsra_scan_in_pos.o\
  lsra_scan_in_prio.o\
  lt_interf_graph.o\
  lt_prio_mgr.o\
  lifetime.o\
  targinfo_mgr.o\
  ir_vrp.o\
  ir_dse.o\
  linear_scan.o\
  ir_vect.o\
  derivative.o\
  prologue_epilogue_inserter.o\
  arg_passer.o\
  gp_adjustment.o\
  br_opt.o\
  workaround.o\
  dynamic_stack.o\
  var2offset.o\
  ir_reloc_mgr.o\
  igoto_opt.o
else ifneq ($(findstring $(REF_TARGMACH_INFO), true),)
  $(info "CONDBR:opt/Makefile.inc:REF_TARGMACH_INFO=$(REF_TARGMACH_INFO)")
  OPT_OBJS+=\
  lsra_impl.o\
  lsra_scan_in_pos.o\
  lsra_scan_in_prio.o\
  lt_interf_graph.o\
  lt_prio_mgr.o\
  lifetime.o\
  targinfo_mgr.o\
  linear_scan.o\
  prologue_epilogue_inserter.o\
  arg_passer.o\
  gp_adjustment.o\
  br_opt.o\
  workaround.o\
  dynamic_stack.o\
  var2offset.o\
  ir_reloc_mgr.o\
  igoto_opt.o
else ifneq ($(findstring $(TARG), FOR_RISCV FOR_TECO),)
  $(info "CONDBR:opt/Makefile.inc:TARG=$(TARG)")
  OPT_OBJS+=\
  lsra_impl.o\
  lsra_scan_in_pos.o\
  lsra_scan_in_prio.o\
  lt_interf_graph.o\
  lt_prio_mgr.o\
  lifetime.o\
  targinfo_mgr.o\
  linear_scan.o\
  prologue_epilogue_inserter.o\
  arg_passer.o\
  gp_adjustment.o\
  br_opt.o\
  workaround.o\
  dynamic_stack.o\
  var2offset.o\
  ir_reloc_mgr.o\
  igoto_opt.o
endif

CFLAGS+=-Wno-unknown-pragmas
