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
  igoto_opt.o\
  kernel_adjustment.o

