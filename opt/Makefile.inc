OPT_OBJS +=\
$(SRC)/opt/cfs_opt.o\
$(SRC)/opt/dbg.o\
$(SRC)/opt/goto_opt.o\
$(SRC)/opt/if_opt.o \
$(SRC)/opt/du.o\
$(SRC)/opt/ir.o\
$(SRC)/opt/ir_bb.o\
$(SRC)/opt/ir_builder.o\
$(SRC)/opt/ir_du.o\
$(SRC)/opt/ir_cfg.o\
$(SRC)/opt/ir_simp.o\
$(SRC)/opt/ir_gvn.o\
$(SRC)/opt/ir_rce.o\
$(SRC)/opt/ir_dce.o\
$(SRC)/opt/ir_cp.o\
$(SRC)/opt/ir_lcse.o\
$(SRC)/opt/ir_gcse.o\
$(SRC)/opt/ir_licm.o\
$(SRC)/opt/ir_middle_opt.o\
$(SRC)/opt/ir_high_opt.o\
$(SRC)/opt/ir_expr_tab.o\
$(SRC)/opt/cdg.o\
$(SRC)/opt/ir_refine.o\
$(SRC)/opt/ir_rp.o\
$(SRC)/opt/ir_aa.o\
$(SRC)/opt/ir_ssa.o\
$(SRC)/opt/ir_mdssa.o\
$(SRC)/opt/mdssainfo.o\
$(SRC)/opt/label.o\
$(SRC)/opt/data_type.o \
$(SRC)/opt/option.o\
$(SRC)/opt/region.o\
$(SRC)/opt/region_mgr.o\
$(SRC)/opt/util.o\
$(SRC)/opt/var.o\
$(SRC)/opt/md.o\
$(SRC)/opt/loop.o\
$(SRC)/opt/cfs_mgr.o\
$(SRC)/opt/pass_mgr.o\
$(SRC)/opt/inliner.o\
$(SRC)/opt/ipa.o\
$(SRC)/opt/callg.o\
$(SRC)/opt/ir_loop_cvt.o\
$(SRC)/opt/refine_duchain.o\
$(SRC)/opt/prssainfo.o\
$(SRC)/opt/scalar_opt.o\
$(SRC)/opt/liveness_mgr.o\
$(SRC)/opt/mdliveness_mgr.o\
$(SRC)/opt/du_helper.o\
$(SRC)/opt/symtab.o\
$(SRC)/opt/logmgr.o\
$(SRC)/opt/ai.o\
$(SRC)/opt/gscc.o\
$(SRC)/opt/lftr.o\
$(SRC)/opt/ir_ivr.o\
$(SRC)/opt/md_mgr.o\
$(SRC)/opt/solve_set.o\
$(SRC)/opt/infer_type.o\
$(SRC)/opt/attachinfo_mgr.o\
$(SRC)/opt/analysis_instr.o\
$(SRC)/opt/invert_brtgt.o\
$(SRC)/opt/ir_helper.o\
$(SRC)/opt/gr_helper.o\
$(SRC)/opt/mdssalive_mgr.o\
$(SRC)/opt/insert_guard_helper.o\
$(SRC)/opt/optctx.o\
$(SRC)/opt/ir_dump.o\
$(SRC)/opt/ir_dump_ext.o\
$(SRC)/opt/ir_mgr.o\
$(SRC)/opt/ir_mgr_ext.o\
$(SRC)/opt/ir_verify.o\
$(SRC)/opt/ir_verify_ext.o\
$(SRC)/opt/ir_decl.o\
$(SRC)/opt/ir_desc.o\
$(SRC)/opt/ir_utils.o\
$(SRC)/opt/cfg_lifting.o

ifeq ($(FOR_IP), true)
  $(info "ADD FOR-IP OBJS")
  OPT_OBJS +=\
  $(SRC)/opt/lsra_impl.o\
  $(SRC)/opt/lsra_scan_in_pos.o\
  $(SRC)/opt/lsra_scan_in_prio.o\
  $(SRC)/opt/lt_interf_graph.o\
  $(SRC)/opt/lt_prio_mgr.o\
  $(SRC)/opt/lifetime.o\
  $(SRC)/opt/targinfo_mgr.o\
  $(SRC)/opt/ir_vrp.o\
  $(SRC)/opt/ir_dse.o\
  $(SRC)/opt/linear_scan.o
endif

CFLAGS += -Wno-unknown-pragmas
