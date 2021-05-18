CC := $(shell which clang++ > /dev/null)
ifndef CC
  CC = $(if $(shell which clang), clang, gcc)
endif

include reader/Makefile.inc
include com/Makefile.inc
include opt/Makefile.inc

#COM_OBJS +=\
#com/ltype.o \
#com/comf.o \
#com/smempool.o \
#com/agraph.o \
#com/sgraph.o \
#com/rational.o \
#com/linsys.o \
#com/xmat.o \
#com/strbuf.o \
#com/bigint.o \
#com/birational.o \
#com/example/testbs.o \
#com/flty.o \
#com/bs.o\
#com/scc.o\
#com/sort.o\
#com/diagnostic.o
#
#OPT_OBJS +=\
#opt/cfs_opt.o\
#opt/dbg.o\
#opt/goto_opt.o\
#opt/if_opt.o \
#opt/ir.o\
#opt/ir_bb.o\
#opt/ir_du.o\
#opt/du.o\
#opt/ir_cfg.o\
#opt/ir_simp.o\
#opt/ir_gvn.o\
#opt/ir_rce.o\
#opt/ir_dce.o\
#opt/ir_cp.o\
#opt/ir_lcse.o\
#opt/ir_gcse.o\
#opt/ir_licm.o\
#opt/ir_ivr.o\
#opt/ir_middle_opt.o\
#opt/ir_high_opt.o\
#opt/ir_expr_tab.o\
#opt/cdg.o\
#opt/ir_refine.o\
#opt/refine_duchain.o\
#opt/ir_rp.o\
#opt/ir_aa.o\
#opt/ir_ssa.o\
#opt/ir_mdssa.o\
#opt/mdssainfo.o\
#opt/label.o\
#opt/data_type.o \
#opt/option.o\
#opt/region.o\
#opt/region_mgr.o\
#opt/util.o\
#opt/var.o\
#opt/md.o\
#opt/cfs_mgr.o\
#opt/pass_mgr.o\
#opt/inliner.o\
#opt/ipa.o\
#opt/callg.o\
#opt/loop.o\
#opt/ir_loop_cvt.o\
#opt/symtab.o\
#opt/prssainfo.o\
#opt/scalar_opt.o\
#opt/mdliveness_mgr.o\
#opt/liveness_mgr.o\
#opt/du_helper.o\
#opt/logmgr.o\
#opt/gscc.o
#
#GRREADER_OBJS += \
#reader/ir_parser.o\
#reader/ir_lex.o\
#reader/grreader.o

ifdef WIN
#Add predefined macro if build host is Windows.
CFLAGS += -D_ON_WINDOWS_
endif

CFLAGS += -DFOR_DEX -D_DEBUG_ -D_SUPPORT_C11_ -O2 -g2 -Wno-unknown-pragmas -Wno-write-strings -Wsign-promo \
        -Wsign-compare -Wpointer-arith -Wno-multichar -Winit-self -Wswitch

ifneq (,$(filter $(CC),g++ gcc))
	CFLAGS += -Wstrict-aliasing=3 -finline-limit=10000000
endif

all: com_objs opt_objs reader_objs
	ar rcs libxoc.a $(COM_OBJS) $(OPT_OBJS) $(READER_OBJS)
	@echo "success!!"

INC=-I .
%.o:%.cpp
	@echo "build $<"
	$(CC) $(CFLAGS) $(INC) -c $< -o $@

com_objs: $(COM_OBJS)
opt_objs: $(OPT_OBJS)
reader_objs: $(READER_OBJS)

clean:
	@find ./ -name "*.gcda" | xargs rm -f
	@find ./ -name "*.gcno" | xargs rm -f
	@find ./ -name "*.o" | xargs rm -f
	@find ./ -name "*.a" | xargs rm -f
	@find ./ -name "*.dot" | xargs rm -f
	@find ./ -name "*.exe" | xargs rm -f
	@find ./ -name "*.elf" | xargs rm -f
	@find ./ -name "*.out" | xargs rm -f
	@find ./ -name "*.tmp" | xargs rm -f
	@find ./ -name "*.vcg" | xargs rm -f
	@find ./ -name "*.cxx" | xargs rm -f
	@find ./ -name "*.asm" | xargs rm -f
	@find ./ -name "*.swp" | xargs rm -f
	@find ./ -name "*.swo" | xargs rm -f
	@find ./ -name "*.log" | xargs rm -f
	@find ./ -name "*.LOGLOG" | xargs rm -f
	@find ./ -name "LOGLOG" | xargs rm -f

