OPT_OBJS+=\
preana.o\
cfs_opt.o\
status.o\
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
ir_bcp.o\
ir_lcse.o\
ir_gcse.o\
ir_licm.o\
ir_middle_opt.o\
ir_high_opt.o\
ir_expr_tab.o\
cdg.o\
eval.o\
ir_refine.o\
ir_rp.o\
ir_aa.o\
ssaregion.o\
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
pass.o\
inliner.o\
ipa.o\
callg.o\
ir_loop_cvt.o\
refine_duchain.o\
prssainfo.o\
scalar_opt.o\
apply_to_region.o\
liveness_mgr.o\
md_liveness_mgr.o\
pr_liveness_mgr.o\
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
alge_reasscociate.o\
analysis_instr.o\
invert_brtgt.o\
ir_helper.o\
gr_helper.o\
loop_dep_ana.o\
mdssalive_mgr.o\
insert_guard_helper.o\
optctx.o\
cfg.o\
ir_dump.o\
ir_dump_ext.o\
ir_mgr.o\
ir_mgr_ext.o\
ir_verify.o\
ir_verify_ext.o\
ir_decl.o\
ir_decl_ext.o\
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
dwarf.o\
targinfo_handler.o

ifeq ($(TARG),)
  $(error "NEED TARG INFO:e.g:make TARG=FOR_DEX TARG_DIR=../dex")
endif

ifeq ($(TARG_DIR),)
  $(error "NEED TARG_DIR INFO:e.g:make TARG=FOR_DEX TARG_DIR=../dex")
endif

ifeq ($(ROOT_DIR),)
  $(error "NEED ROOT_DIR:e.g:make ROOT_DIR=~/xoc")
endif

CURFILE=$(ROOT_DIR)/opt/Makefile.inc
$(info "CURFILE: $(CURFILE)")

ifeq ($(FOR_IP), true)
  $(info "INCLUDE: IN $(CURFILE), INCLUDE $(ROOT_DIR)/opt/Makefile.ip.inc")
  include $(ROOT_DIR)/opt/Makefile.ip.inc
  $(info "INCLUDE: IN $(CURFILE), INCLUDE \
          $(ROOT_DIR)/opt/Makefile.ref_targmach_info.inc")
  include $(ROOT_DIR)/opt/Makefile.ref_targmach_info.inc
else ifneq ($(findstring $(REF_TARGMACH_INFO), true),)
  $(info "INCLUDE: IN $(CURFILE), INCLUDE \
          $(ROOT_DIR)/opt/Makefile.ref_targmach_info.inc")
  include $(ROOT_DIR)/opt/Makefile.ref_targmach_info.inc
endif

ifneq ($(findstring $(TARG), FOR_RISCV FOR_TECO),)
  TARG_DIR_NAME := $(notdir $(TARG_DIR))
  opt_ext_file = $(ROOT_DIR)/$(TARG_DIR_NAME)/Makefile.opt.ext.inc
  ifneq ($(wildcard $(opt_ext_file)),$(opt_ext_file))
    $(error "OPT_EXT_FILE $(opt_ext_file) DOES NOT EXIST.")
  endif
  $(info "INCLUDE: IN $(CURFILE), INCLUDE $(opt_ext_file)")
  include $(opt_ext_file)
endif

CFLAGS+=-Wno-unknown-pragmas
