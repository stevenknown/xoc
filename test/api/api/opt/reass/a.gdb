#compiler
file test.exe
#file pcxac/pcxac

b m518087
b m116038
b main
#b xoc::AlgeReassociate::foldConstLastTwoOp
#b xoc::LICM::perform
#b xoc::SolveSetMgr::perform
#b xoc::UseDefMgr::allocVMD
#b InsertPreheaderMgr::checkAndInsertGuardBB
b Region::rebuildCFGAndDUChainIfNeeded
b ActMgr::allocStrBuf
b if_conversion.cpp:1759
b xoc::BitSetMgr::create

run  -O3  -no-lsra  -no-if_cvs   /home/zhenyu/x/test/compile/vect12.c.i -dump a.log -dump-all -no-cg
#run -O0 -o /home/zhenyu/x/test/compile/ansic.c.asm  -no-lsra  -no-if_cvs   /home/zhenyu/x/test/compile/ansic.c.i -dump a.log -dump-all
#run -O0 -o /home/zhenyu/x/test/compile/20020318-1.c.asm  -no-lsra  -no-if_cvs   /home/zhenyu/x/test/compile/20020318-1.c.i
#run -O3 /home/zhenyu/x/test/compile/lftr2.c  -O3 -no-dce -no-rp -no-licm -no-cp -no-lis -lftr -dump-lftr -dump-ivr -dump-all -dump a.log
#run -O3 /home/zhenyu/x/test/compile/lftr2.c  -no-lsra  -no-if_cvs -dump a.log -dump-all
#run -O0 /home/zhenyu/x/test/compile/atol.c.i  -no-lsra  -no-if_cvs -dump a.log -dump-all
#run -O0 -o /home/zhenyu/x/test/compile/pr29250.c.asm  -no-lsra  -no-if_cvs   /home/zhenyu/x/test/compile/pr29250.c.i -dump a.log -dump-all
#run -O0 -o /home/zhenyu/x/test/compile.gr/no_classic_prdu/extop.gr.asm -no-vect -no-lsra -no-cg -no-if_cvs   /home/zhenyu/x/test/compile.gr/no_classic_prdu/extop.gr 
#run  -O0 -O3 -only-vect -gcse -cp_aggr -dce_aggr -rce -cfgopt -no-cg   -licm   -licm_no_guard   -dce_aggr   -no-lowest_height -dump-vect -o /home/zhenyu/x/test/compile/vect18_5.c.asm  -no-lsra  -no-if_cvs   /home/zhenyu/x/test/compile/vect18_5.c.i -dump a.log -dump-all
#run -O0 -O3 -only-vect -cp_aggr -reass -dce_aggr -dump-vect -dump-reass -dump-if_cvs -o /home/zhenyu/x/test/compile/ifcvs_reass.c.asm  -no-lsra  -no-if_cvs   /home/zhenyu/x/test/compile/ifcvs_reass.c.i
#run -O3 -no-vect -dump a.log -dump-all  /home/zhenyu/x/test/compile/duref.c.i
#run -O3 -no-vect -dump a.log -only-if_cvs /home/zhenyu/x/test/exec/zzbug2.c -no-cg -dump-all
#run -O3 -no-vect -dump a.log /home/zhenyu/x/test/compile.gr/no_classic_prdu/dce_prssa.gr -time
#run -O3 -no-vect -no-if_cvs -only-licm -rce -dump a.log -dump-all /home/zhenyu/x/test/exec/zzbug.c
#run -dump-all -dump a.log /home/zhenyu/x/test/compile/pr28675.c -O3 -lowest_height -nonprdu -prdu -mdssa -prssa -no-vect -no-lsra -no-cg
#run -O3  -prdu -nonprdu -no-mdssa -no-prssa -no-cg   /home/zhenyu/x/test/compile.gr/gvn.gr -dump-all -dump a.log
#run -O3 -prssa -mdssa -prdu -nonprdu -no-cg   /home/zhenyu/x/test/compile.gr/gvn.gr -dump-all -dump a.log
#run -O3 -licm -dump-licm -prdu -nonprdu -lowest_height -no-cg -no-prssa -O3 -no-migen -no-lsra -no-vect -no-lsra -no-cg  /home/zhenyu/x/test/compile.gr/licm.gr -dump-all -dump a.log
#run -O3 -only-dse -dump a.log -dump-dse /home/zhenyu/x/test/compile/dse_mult_def.c -dump-all
#run -O3 -only-dse -dump a.log -dump-dse /home/zhenyu/x/test/compile/dse_local_alias.c -dump-all
#run -O3 -only-dse -dump a.log -dump-dse /home/zhenyu/x/test/compile/dse_local.c -dump-all
#run -O3 -no-vect -no-if_cvs -only-licm -include_region = FLOYD -dump a.log -dump-all /home/zhenyu/x/test/exec/a7_3.c
#run -O3 -no-vect -no-if_cvs -only-licm -include_region = FLOYD -dump a.log -dump-all /home/zhenyu/x/test/exec/a7_3.c -o bug.asm
#run -dump a.log -dump-all -O3 -only-licm -no-vect -no-lsra -no-cg ~/x/test/exec/zzbug.c
#run /home/zhenyu/x/test/compile.gr/cp_avail4.gr -O3 -mdssa -prssa -no-vect -no-lsra -no-cg -dump a.log -dump-all
#run /home/zhenyu/x/test/compile.gr/cp_avail3.gr -O3 -mdssa -prssa -no-vect -no-lsra -no-cg -dump a.log -dump-all
#run /home/zhenyu/x/test/compile.gr/cp_avail2.gr -O3 -mdssa -prssa -no-vect -no-lsra -no-cg -dump a.log -dump-all
#run /home/zhenyu/x/test/compile.gr/cp_avail.gr -O3 -nonprdu -prdu -mdssa -prssa -no-vect -no-lsra -no-cg -dump a.log -dump-all
#run /home/zhenyu/x/test/compile.gr/licm_dd.gr -O3 -nonprdu -prdu -mdssa -prssa -no-vect -no-lsra -no-cg -dump a.log -dump-all -no-reuse_ir -no-cg
#run /home/zhenyu/x/test/compile.gr/licm.gr -O3 -no-vect -no-lsra -no-cg -dump a.log -dump-irid -no-reuse_ir -dump-all
#run /home/zhenyu/x/test/compile/zzbug.c -O3 -only-licm -no-vect -no-lsra -no-cg -dump a.log -dump-irid -no-reuse_ir -dump-all
#run /home/zhenyu/x/test/compile/block.c -O3 -only-licm -no-vect -no-lsra -no-cg -dump a.log -dump-irid -no-reuse_ir
#run /home/zhenyu/x/test/compile/licm_clean_mdssa.c -O3 -only-licm -no-vect -no-lsra -no-cg -dump a.log -dump-irid -no-reuse_ir -dump-all
#run /home/zhenyu/x/test/compile/licm_revise_ssa_in_preheader.c -O3 -only-licm -no-vect -no-lsra -no-cg -dump a.log -dump-irid -no-reuse_ir -dump-all
#run /home/zhenyu/x/test/compile/licm_insert_mdphi.c -O3 -no-vect -no-lsra -no-cg -dump a.log -dump-irid -no-reuse_ir -dump-all
#run /home/zhenyu/x/test/compile.gr/alias_loop_carry.gr -O3 -nonprdu -prdu -mdssa -prssa -no-vect -no-lsra -no-cg -dump a.log -dump-all -no-reuse_ir -no-cg
#run /home/zhenyu/x/test/compile.gr/lsra_regset_alias2.gr -O3 -nonprdu -prdu -mdssa -prssa -no-vect  -dump a.log -dump-all -no-reuse_ir -no-cg
#run /home/zhenyu/x/test/compile.gr/gcse3.gr -O3 -mdssa -prssa -only-licm -no-vect -no-lsra -no-cg -dump a.log -dump-all -no-reuse_ir
#run /home/zhenyu/x/test/compile.gr/ssa_insert_dd_chain.gr -O3 -mdssa -prssa -no-vect -no-lsra -no-cg -dump a.log -dump-all -no-reuse_ir
#run /home/zhenyu/x/test/compile/licm_dd.c -O3 -nonprdu -prdu -mdssa -prssa -no-vect -no-lsra -no-cg -dump a.log -dump-all
#run /home/zhenyu/x/test/compile/licm_dd.c -O3 -only-licm -no-vect -no-lsra -no-cg -dump a.log -dump-all
#run /home/zhenyu/x/test/compile.gr/licm_dd.gr -O3 -nonprdu -prdu -mdssa -prssa -no-vect -no-lsra -no-cg -dump a.log -dump-all
#run /home/zhenyu/x/test/compile.gr/no_classic_prdu/lsra_bug.gr -O3  -no-migen -no-lsra -no-vect -no-lsra -no-cg -dump a.log -no-if_cvs
#run /home/zhenyu/x/test/compile.gr/licm_low_perf.gr -O3 -nonprdu -prdu -mdssa -prssa -no-vect -no-lsra -no-cg -dump a.log -dump-all
#run /home/zhenyu/x/test/compile.gr/redundant_label.gr -O3 -nonprdu -prdu -mdssa -prssa -no-vect -no-lsra -no-cg -dump a.log -dump-all
#run /home/zhenyu/x/test/compile.gr/lsra_regset_alias2.gr -O3 -no-migen -no-lsra -no-vect -no-lsra -no-cg -dump a.log -dump-all
#run /home/zhenyu/x/test/8cc-master/gen.c.i -O3  -O3 -no-vect -no-lsra -no-cg  -dump a.log
#run ~/x/test/compile.gr/licm_dd.gr -O3 -only-licm -dump-all -dump a.log -no-cg
#run ~/x/test/compile.gr/cp_avail.gr -O3 -dump-all -dump a.log
#run ../test/pcx/convolution_forward_2d_k1_little_good.pcx -O0 -arch-t1 -dump-irid -dump a.log
#run -dump a.log -dump-all -O3 -no-vect -no-lsra -no-cg ~/x/test/exec/evn.c -O3  -only-gcse 
#run -dump a.log -dump-all -O3 -no-vect -no-lsra -no-cg /data/compiler/ligao/x/test/compile/array_access.c -O3 
#run -dump a.log -dump-all -O3 -no-vect -no-lsra -no-cg /data/compiler/ligao/x/test/compile.gr/licm.gr -O3 -licm -dump-licm -prdu -nonprdu -lowest_height  -no-cg  -no-prssa
#run -dump a.log -dump-all -O3 -no-vect -no-lsra -no-cg /data/compiler/ligao/x/test/compile.gr/combine_br.gr
#run -dump a.log -dump-all -O3 -no-vect -no-lsra -no-cg /data/compiler/ligao/x/test/compile.gr/bool.gr
#run -dump a.log -dump-all -O3 -no-vect -no-lsra -no-cg /data/compiler/ligao/x/test/8cc-master/cpp.c.i
#run -dump-all -dump a.log -O3 -dump-all -dump a.log -no-vect -no-lsra -only-gcse   /data/compiler/ligao/x/test/exec/zzbug6.c
#run -dump-all -dump a.log -O3 -no-vect -no-lsra -no-cg /data/compiler/ligao/x/test/exec/gvn.c 
#run -dump-all -dump a.log -O3 -no-vect -no-lsra -no-cg /data/compiler/ligao/x/test/8cc-master/zzbug.c  -only-rce
#run -dump-all -dump a.log -O3 -no-vect -no-lsra -no-cg /data/compiler/ligao/x/test/8cc-master/zzbug_2.c  -only-rce -gcse -cp -dce_aggr
#run -O3 -O3 -dump-all -dump a.log -no-vect -no-lsra -only-gcse /data/compiler/ligao/x/test/exec/zzbug.c 
#run -O3 -lowest_height -no-vect -prmode -no-lsra /data/compiler/ligao/x/test/compile.gr/no_classic_prdu/cfg_opt3_tramp.gr -dump a.log -dump-all -no-cg
#run -O3 -no-migen -no-lsra -no-vect -no-cg -only-dce -dce_aggr -O3 -nonprdu -prdu -mdssa -prssa -no-vect -no-lsra -no-cg /data/compiler/ligao/x/test/compile.gr/dce_prssa.gr
#run -dump a.log -O3 -no-vect -no-lsra -no-cg ~/x/test/compile.gr/ra_impacted_by_vector_reg.gr -only-reass
#run -dump-all -dump a.log -O3 -no-vect -no-lsra -no-cg ~/x/test/compile/samedef.c -only-cp
#run ../test/exec/zzbug.c -O3 -only-licm -dce_aggr -no-lsra -no-vect -no-cg -dump a.log -dump-all
#run ../test/exec/zzbug.c -O3 -only-licm -no-lsra -no-vect -no-cg -dump a.log -dump-all
#run ../test/exec/alias_analysis.c -O3 -only-licm -no-lsra -no-vect -no-cg -dump a.log 
#run ../test/compile/ansic.c -O3 -no-lsra -no-vect -no-cg -dump a.log
#run ../test/compile/zzbug2.c -O3 -no-lsra -no-vect -no-cg -dump a.log -only-licm -reass -if_cvs -dump-all
#run -O3 -no-migen -no-lsra -no-vect -no-cg -only-dce -dce_aggr -O3 -nonprdu -prdu -mdssa -prssa -no-vect -no-lsra -no-cg  /data/compiler/ligao/x/test/compile.gr/dce_prssa.gr
#run -dump-all -dump a.log -O3 -nonprdu -prdu -mdssa -prssa -no-vect -no-lsra -no-cg  ~/x/test/compile/if_cvs_mdphi.c
#run -dump a.log -O3 -nonprdu -prdu -mdssa -prssa -no-vect -no-lsra -no-cg ~/x/test/compile.gr/ra_impacted_by_vector_reg.gr
#run -dump-all -dump a.log -O3 -no-vect -no-lsra -lowest_height -prmode ~/x/test/exec/zzbug.c -no-cg -only-reass -cp
#run -dump-all -dump a.log -O3 -no-vect -no-lsra -no-if_cvs ~/x/test/chibicc-main/zzbug.c -no-cg
#run -dump-all -dump a.log -O3 -no-vect -no-lsra -no-if_cvs ~/x/test/chibicc-main/preprocess.c.i -no-cg
#run -dump-all -dump a.log -O3 -only-rp -gcse -no-vect -no-lsra -no-if_cvs ~/x/test/exec/zzbug.c -no-cg
#run -dump-all -dump a.log -O3 -no-vect -no-lsra -no-if_cvs ~/x/test/exec/zzbug.c
#run -dump-all -dump a.log -O3 -nonprdu -prdu -mdssa -prssa -no-vect -no-lsra -no-cg  ~/x/test/compile/if_cvs_mdphi.c
#run -dump-all -dump a.log -O3 -nonprdu -prdu -mdssa -prssa -no-vect -no-lsra -no-cg  ~/x/test/8cc-master/gen.c.i
#run -dump-all -dump a.log -O3 -nonprdu -prdu -mdssa -prssa -no-vect -no-lsra -no-cg  ~/x/test/compile.gr/no_classic_prdu/extop.gr
#run -dump-all -dump a.log -O3 -nonprdu -prdu -mdssa -prssa -no-vect -no-lsra -no-cg  ~/x/test/compile.gr/dce_bug.gr
#run -dump-all -dump a.log -O3 -nonprdu -prdu -mdssa -prssa -no-vect -no-lsra -no-cg  ~/x/test/compile.gr/cvt_gvn.gr
#run -dump-all -dump a.log -O3 -nonprdu -prdu -mdssa -prssa -no-vect -no-lsra -no-cg  ~/x/test/compile.gr/du.gr
#run -dump-all -dump a.log -O3 -no-migen -no-lsra -no-vect -no-lsra -no-cg  ~/x/test/compile.gr/inner_region.gr
#run -dump-all -dump a.log -O3 -no-migen -no-lsra -no-vect -no-lsra -no-cg ~/x/test/compile.gr/array.gr
#run ~/x/test/compile/licm_guard.c -dump-all -dump a.log -O3 -lowest_height -only-licm -prssa -mdssa 
#run ~/x/test/compile/licm11.c -O3 -no-vect -no-lsra -no-cg -dump-all -dump a.log
#run ~/x/test/compile/20020116-1.c -O3 -only-licm -dump-licm -dce -dump-dce -dce_aggr -rp -cp -lftr -dump-rp -dump-dumgr -cfgopt -mdssa -dump-mdssa -prssa -dump-prssa -rce -dump-all -dump a.log
#run ~/x/test/compile/alias13.c -O3 -no-dce -no-dce_aggr -no-lftr -no-cfgopt -no-cp -no-cp_aggr -no-licm -no-rp -prssa -mdssa -prdu -nonprdu -dump-all -dump a.log
#run ~/x/test/compile/rp0.c -O3 -only-rp -cfgopt -gvn -dce_aggr -no-vect -no-lsra -dump-all -dump a.log
#run ~/x/test/compile/lcse.c -O3 -only-lcse -no-if_cvs -dump-all -dump a.log -no-vect -no-lsra
#run ~/x/test/compile/lcse_ist.c -O3 -only-lcse -no-if_cvs -dump-all -dump a.log -no-vect -no-lsra
#run ~/x/test/compile/lcse_ist.c -O3 -only-lcse -dump-all -dump a.log -no-vect -no-lsra -no-cg
#run ~/x/test/compile/gvn6.c -O3 -only-cp -cp_aggr -lcse -gvn -dump-all -dump a.log -no-vect -no-lsra -no-cg
#run -O3  ~/x/test/compile.gr/gcse4.gr -dump a.log -dump-all -no-cg -O3 -only-lcse
#run -O3 -only-ivr ~/x/test/compile.gr/ivr_invariant_step.gr -dump a.log -dump-all -no-cg
#run -O3  ~/x/test/compile.gr/reass.gr -dump a.log -dump-all -no-cg
#run -O3  ~/x/test/compile.gr/ra_impacted_by_vector_reg.gr -dump a.log -dump-all -no-cg
#run -O3  ~/x/test/compile.gr/zzbug.gr -dump a.log -dump-all
#run -O3 -only-if_cvs ~/x/test/compile.gr/ivr_cr3.gr -ivr -cp_aggr -reass -dump a.log -dump-all
#run ../test/compile/licm2.c -O3 -nonprdu -prdu -no-mdssa -no-prssa -dump-all -dump a.log -no-vect -no-lsra -no-cg -only-if_cvs
#run ../test/compile/20020604-1.c -O3 -nonprdu -prdu -no-mdssa -no-prssa -dump-all -dump a.log -no-vect -no-lsra -no-cg -only-rp -dce -licm
#run ../test/compile/20020530-1.c -O3 -nonprdu -prdu -no-mdssa -no-prssa -dump-all -dump a.log -no-vect -no-lsra -only-if_cvs -no-cg
#run ../test/compile/nested_ternary.c -O3 -dump-all -dump a.log -no-vect -no-lsra -only-dce -if_cvs -cfgopt -cp
#run ../test/compile/licm10.c -O3 -dump-all -dump a.log -only-licm -if_cvs
#run ../test/compile/sel_det.c -no-cg -dump-all -dump a.log -O3 -only-cp -if_cvs -vect -cp_aggr -reass -gcse
#run ../test/compile/zzbug.c -O3 -no-cg -dump-all -dump a.log  -O3 -only-vect -cp_aggr  -reass -dce_aggr
#run ../test/compile/zzbug.c -O3 -only-vect -rce -cp_aggr -rp  -no-cg -dump-all -dump a.log
#run ../test/compile/vect_if_cvs.c -no-cg -dump-all -dump a.log -O3 -only-vect -rce -rp -if_cvs
#run ../test/compile/dce_dominfo2.c -O3 -no-cg -lowest_height -prdu -nonprdu -no-prssa -only-licm -rce -rp -dce -dce_aggr -cp_aggr -cfgopt  -dump-all -dump a.log
#run ../test/exec/cse.c -O3 -nonprdu -prdu -no-vect -no-lsra -no-cg -dump-all -dump a.log
#run ../test/exec/zzbug_deadloop3.c -O3 -no-vect -no-lsra -no-cg  -only-cp -reass -lowest_height -prmode -dump-all -dump a.log
#run ../test/exec/c4.org.c -O3 -lowest_height -prmode -no-cg -dump-all -dump a.log
#run ../test/exec/facts.c -O3 -dump-all -dump a.log
#run ../test/exec/a5_1.c -O3 -only-if_cvs -cp -dump-all -dump a.log
#run ../test/exec/if_cvs_const.c -O3 -only-if_cvs -cp -dump-all -dump a.log
#run ../test/exec/select_exp_list.c -O3 -dump-all -dump a.log
#run ../test/exec/zzbug.c -O3 -only-rce -cp -dump-all -dump a.log
#run ../test/exec/zzbug.c -O3 -only-dce -if_cvs -cp -reass -dump-all -dump a.log
#run ../test/exec/conversion.bug.c -O3 -only-dce -if_cvs -cp -reass -dump-all -dump a.log
#run ../test/compile/reass_no.c -O3 -only-reass -dump-all -dump a.log
#run ../test/exec/reass.c -O3 -only-reass -dump-all -dump a.log
#run ../test/exec/if_cvs2.c -O3 -no-lsra -dump-all -dump a.log
#run ../test/exec/if_cvs.c -O3 -no-lsra -dump-all -dump a.log
#run ../test/exec/if_cvs.c -O3 -only-if_cvs -cg -no-lsra -dump-all -dump a.log
#run ../test/compile/vect_if3.c -O3 -vect -cp -cp_aggr -gvn -licm -cg -no-lsra -dump-all -dump a.log
#run ../test/compile/switch.c -O3 -vect -cp -cp_aggr -gvn -licm -no-cg -no-lsra -dump-all -dump a.log
#run reader/test.gr -O3 -no-lsra -no-cg -dump-all -dump a.log
#run ../test/compile.gr/ra_impacted_by_vector_reg.gr -O3 -no-lsra -no-cg -dump-all -dump a.log
#run ../test/compile/vect18_5.c -O3 -vect -no-cp -no-cp_aggr -gvn -licm -no-cg -no-lsra -dump-all -dump a.log
#run ../test/pcx/mix_use_sc_vec.pcx -dump a.log -O0
#run ../test/exec/c4.org.c -O3 -lsra -no-vect -dump a.log -O0 -dump-all
#run ../test/compile.gr/no_classic_prdu/no_reloadpos_ir.gr -O3 -lsra -no-vect -no-cg -dump-all -dump a.log 
#run ../test/compile.gr/no_classic_prdu/lsra_bug.gr -O3 -lsra -no-vect -no-cg -dump-all
#run ../test/pcx/cfgopt_tramp.pcx -dump a.log -O0 -dump-all
#run ../test/compile.gr/doloop_cnt.gr -O3 -no-lsra -no-vect -no-cg -dump a.log -dump-all
#run ../test/chibicc-main/preprocess.c.i -O3 -no-lsra -no-vect -no-cg
#run ../test/chibicc-main/parse.c.i -O3 -no-lsra -no-vect -no-cg
#run ../test/pcx/classic_du.pcx -dump a.log -O0 -dump-all
#run ../test/exec/facts.c -O3 -only-cp -gcse -no-lsra -no-vect -dump a.log -dump-all -include_region = "main"
#run ../test/exec/test_shift.c -O3 -no-lsra -no-vect -dump a.log -dump-all
#run ../test/exec/qualifer.c -O3 -only-gcse -no-lsra -no-vect -dump a.log -dump-all
#run ../test/exec/gcse_cvt.c -O3 -only-gcse -no-lsra -no-vect -dump a.log -dump-all -dump-gr
#run ../test/exec/conversion.org.c -O3 -only-gcse -no-lsra -no-vect -dump a.log -dump-all
#run ../test/compile.gr/lsra_bug.gr -O3 -no-migen -no-lsra -no-vect -no-lsra -dump a.log -dump-irid -dump-all
#run ../test/compile.gr/no_classic_prdu/isomo.gr -dump a.log -O3 -only-gcse -gvn -dump-all
#run ../test/compile.gr/no_classic_prdu/isomo.gr -dump a.log -O3 -only-gcse -gvn -dump-all
#run ../test/compile.gr/licm.gr -O3 -nonprdu -prdu -no-lsra -no-vect -no-cg -dump a.log -dump-all
#run ../test/compile/pr29201.c -dump a.log -O3 -dump-all
#run ../test/compile.gr/gcse_gvn.gr -dump a.log -O3 -dump-all -dump-for_test
#run ../test/pcx/can_remat_lda_case.pcx -dump a.log -O0 -dump-all
#run ../test/exec/array_and_pointer.c -dump a.log -O3 -no-vect -no-lsra -dump-all
#run ../test/exec/bcp.c -dump a.log -O3 -no-vect -no-lsra -dump-all
#run ../test/exec/bcp.c -dump a.log -O3 -only-bcp -rce -gvn -no-cg -dump-bcp -dump-rce
#run ../test/exec/bcp.c -dump a.log -O3 -only-bcp -rce -gvn -no-cg -dump-all
#run ../test/compile.gr/cvt_cse.gr -dump a.log -O3 -only-gcse -gvn -no-cg -dump-all -dump-for_test
#run ../test/compile.gr/extop.gr -dump a.log -O3 -no-cg
#run ../test/compile.gr/extop.gr -arch-t1 -dump a.log -O0

b m518087
b m116038

set print pretty on
set listsize 20

define lxocc
file xocc/xocc.exe
end

define lpcxac
file pcxac/pcxac.exe
end

define prg
p $arg0->getRegionName()
end

define fss
fs src 
end
define fsc
fs cmd
end

define ff
call fflush($arg0)
end

define pcfg
call $arg0->dumpDOT((char const*)0,xoc::IRCFG::DUMP_COMBINE,0)
end

define pir
p dumpIRCombine($arg0, m_rg)
end

define pir1
p dumpIRCombine($arg0, $arg1)
end

define aaa
winh src - 5
end

define sss
winh src + 5
end

define rrr
refresh
end

define ff
call fflush($arg0)
end

define pcfg
p ($arg0)->dumpDOT((CHAR const*)0, xoc::IRCFG::DUMP_COMBINE,0)
end

define pirl
p xoc::dumpIRList($arg0,m_rg,false,0)
end


define pirl1
p xoc::dumpIRList($arg0,$arg1,false,0)
end

define pir
p dumpIRCombine($arg0, m_rg)
end

define pir1
p dumpIRCombine($arg0, $arg1)
end

define pbbl
p $arg0->dumpBBList(1)
end

define pbbl1
p $arg0->dumpBBList($arg1, true)
end

define pline
p m_lexer->m_src_line_num
end

define endump
p g_dump_opt.setDumpAll()
end

define disdump
p g_dump_opt.setDumpNothing()
end

