/*@
Copyright (c) 2013-2021, Su Zhenyu steven.known@gmail.com

All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.
    * Redistributions in binary form must reproduce the above copyright
      notice, this list of conditions and the following disclaimer in the
      documentation and/or other materials provided with the distribution.
    * Neither the name of the Su Zhenyu nor the names of its contributors
      may be used to endorse or promote products derived from this software
      without specific prior written permission.

THIS SOFTWARE IS PROVIDED "AS IS" AND ANY
EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE CONTRIBUTORS BE LIABLE FOR ANY
DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
@*/
#include "xinfer_cominc.h"

namespace xinfer {

class OptWrap;

static bool g_ask_for_help = false;
static CHAR const* g_include_string = nullptr;
static CHAR const* g_exclude_string = nullptr;
CHAR const* g_prompt = nullptr;
CHAR const* g_output_file_name = nullptr; //record the output file name.
CHAR const* g_version = "1.0.0"; //record the exe version.
CHAR const* g_dump_file_name = nullptr;
CHAR const* g_model_name = nullptr;
bool g_dump_tensor_data = false;
bool g_dump_json_parser = false;
bool g_continuation = false;
bool g_dump_safetensors_mgr = false;
bool g_dump_config = false;
bool g_dump_tokenizer = false;
bool g_dump_rtarg = false;
bool g_dump_llminfer = false;
bool g_dump_rtctx = false;
bool g_dump_act = false;
bool g_dump_pieces = false;
bool g_dump_rt_norm = false;
bool g_dump_rt_vec = false;
bool g_dump_rt = false;

//float eps range:1e-10f ~ 1e-14f
//half eps range:1e-7f ~ 1e-8f
//bf16 eps range:1e-7f ~ 1e-8f
float g_softmax_epsilon = 1e-12f;

//-1 means dump all tensor data.
INT g_max_tensor_data_dump_num = 10;
UINT g_max_seq_len = (UINT)-1;
INT g_topk = 40;
HOST_FP g_temperature = 0.7;

xcom::List<CHAR const*> g_safetensors_file_list;
xcom::List<CHAR const*> g_gguf_file_list;
xcom::List<CHAR const*> g_json_file_list;
xcom::List<CHAR const*> g_txt_file_list;

static bool dispatchByPrefix(
    CHAR const* cmdstr, INT argc, CHAR const* argv[],
    INT & i, OUT OptWrap & ow);
static void inferDumpOption(CHAR const* cmdstr);
static void inferStringOption();
static void disableAllDumpOption();

static bool is_json_file(CHAR const* fn)
{
    return xcom::isFileSuffix(fn, "JSON", nullptr);
}


static bool is_safetensors_source_file(CHAR const* fn)
{
    return xcom::isFileSuffix(fn, "SAFETENSORS", nullptr);
}


static bool is_gguf_source_file(CHAR const* fn)
{
    return xcom::isFileSuffix(fn, "GGUF", nullptr);
}


static bool is_txt_file(CHAR const* fn)
{
    return xcom::isFileSuffix(fn, "txt", nullptr);
}


static void disableAllOpt(INT opt_level)
{
    switch (opt_level) {
    case OPT_LEVEL0:
        xoc::g_do_lsra = false;
        //Fall through.
    case OPT_LEVEL1:
    case OPT_LEVEL2:
    case OPT_LEVEL3:
    case SIZE_OPT:
        xoc::g_do_cp = false;
        xoc::g_do_cp_aggressive = false;
        xoc::g_do_bcp = false;
        xoc::g_do_dce = false;
        xoc::g_do_dse = false;
        xoc::g_do_dce_aggressive = false;
        xoc::g_do_alge_reassociate = false;
        xoc::g_do_alge_reassociate_aggressive = false;
        xoc::g_do_alge_distributive = false;
        xoc::g_do_licm = false;
        xoc::g_do_gcse = false;
        xoc::g_do_lcse = false;
        xoc::g_do_if_conversion = false;
        xoc::g_do_rp = false;
        xoc::g_do_lftr = false;
        xoc::g_infer_type = false;
        xoc::g_do_rce = false;
        xoc::g_do_vect = false;
        xoc::g_do_gvn = false;
        break;
    default: UNREACHABLE();
    }
}


static bool process_opt(INT argc, CHAR const* argv[], INT & i)
{
    DUMMYUSE(argc);
    CHAR const* cmdstr = &argv[i][1];
    switch (cmdstr[1]) {
    case '0':
        xoc::g_opt_level = OPT_LEVEL0;
        xoc::g_pass_opt.disableAllPass();
        //Note the essential options will be set at the end.
        break;
    case '1':
        xoc::g_opt_level = OPT_LEVEL1;
        xoc::g_pass_opt.enablePassInLevel1();
        break;
    case '2':
        xoc::g_opt_level = OPT_LEVEL2;
        xoc::g_pass_opt.enablePassInLevel2();
        break;
    case '3':
        xoc::g_opt_level = OPT_LEVEL3;
        xoc::g_pass_opt.enablePassInLevel3();
        break;
    case 's':
    case 'S':
        xoc::g_opt_level = SIZE_OPT;
        xoc::g_pass_opt.enablePassInLevelSize();
        break;
    default:
        xoc::g_opt_level = OPT_LEVEL1;
        xoc::g_pass_opt.enablePassInLevel1();
    }
    i++;
    return true;
}


static bool process_output_file(INT argc, CHAR const* argv[], INT & i)
{
    if (i + 1 < argc && argv[i + 1] != nullptr) {
        g_output_file_name = argv[i + 1];
    }
    i += 2;
    return true;
}


static CHAR const* process_dump(INT argc, CHAR const* argv[], INT & i)
{
    CHAR const* n = nullptr;
    if (i + 1 < argc && argv[i + 1] != nullptr) {
        n = argv[i + 1];
    }
    i += 2;
    return n;
}

typedef enum tagOptKind {
    OPT_KIND_UNDEF = 0,
    OPT_KIND_FUNC, //these options are used to control some functions.
    OPT_KIND_DUMP, //these options are used to control dump.
} OptKind;

//The class represents a wrapper to the extracted object that will
//be modified soon.
class OptWrap {
public:
    bool * boption;
    bool ** boption_ptr;
    OptKind option_kind;
public:
    OptWrap()
    {
        ::memset(this, 0, sizeof(OptWrap));
        boption_ptr = &boption;
    }
};

class BoolOption {
protected:
    class Desc {
    public:
        CHAR const* name;
        bool * option;
        CHAR const* intro;
    };
    static Desc const option_desc[];
    static Desc const dump_option_desc[];
    static Desc const elf_option_desc[];
public:
    static CHAR const* dump_option_prefix;
    static CHAR const* disable_option_prefix;
    static CHAR const* only_option_prefix;
    static CHAR const* elf_option_prefix;
public:
    static UINT getNumOfOption();
    static UINT getNumOfDumpOption();
    static UINT getNumOfELFOption();

    static bool is_option(CHAR const* str, OUT OptWrap & ow)
    {
        ASSERT0(ow.boption_ptr);
        for (UINT i = 0; i < BoolOption::getNumOfOption(); i++) {
            if (::strcmp(BoolOption::option_desc[i].name, str) == 0) {
                *ow.boption_ptr = BoolOption::option_desc[i].option;
                ow.option_kind = OPT_KIND_FUNC;
                return true;
            }
        }
        return false;
    }

    static bool is_dump_option(CHAR const* str, OUT OptWrap & ow)
    {
        ASSERT0(ow.boption_ptr);
        for (UINT i = 0; i < BoolOption::getNumOfDumpOption(); i++) {
            if (::strcmp(BoolOption::dump_option_desc[i].name, str) == 0) {
                *ow.boption_ptr = BoolOption::dump_option_desc[i].option;
                ow.option_kind = OPT_KIND_DUMP;
                return true;
            }
        }
        return false;
    }

    static bool is_elf_option(CHAR const* str, OUT OptWrap & ow)
    {
        ASSERT0(ow.boption_ptr);
        for (UINT i = 0; i < BoolOption::getNumOfELFOption(); i++) {
            if (::strcmp(BoolOption::elf_option_desc[i].name, str) == 0) {
                *ow.boption_ptr = BoolOption::elf_option_desc[i].option;
                return true;
            }
        }
        return false;
    }

    static void dump_usage(StrBuf & output)
    {
        UINT max_name_len = 0;
        for (UINT i = 0; i < BoolOption::getNumOfOption(); i++) {
            max_name_len =
                MAX((UINT)::strlen(
                    BoolOption::option_desc[i].name), max_name_len);
        }
        UINT alignbuflen = max_name_len + 2;

        CHAR * alignbuf = (CHAR*)ALLOCA(alignbuflen);
        for (UINT i = 0; i < BoolOption::getNumOfOption(); i++) {
            UINT j = 0;
            for (; j < ::strlen(BoolOption::option_desc[i].name); j++) {
                alignbuf[j] = BoolOption::option_desc[i].name[j];
            }
            for (; j < alignbuflen - 1; j++) {
                alignbuf[j] = ' '; //pad the rest with blank
            }
            alignbuf[j] = 0;

            output.strcat(
                "\n -%s %s", alignbuf, BoolOption::option_desc[i].intro);
        }
    }

    static void dump_dump_option(StrBuf & output)
    {
        UINT max_name_len = 0;
        for (UINT i = 0; i < BoolOption::getNumOfDumpOption(); i++) {
            max_name_len =
                MAX((UINT)::strlen(BoolOption::dump_option_desc[i].name),
                    max_name_len);
        }
        UINT alignbuflen = max_name_len + 2;

        CHAR * alignbuf = (CHAR*)ALLOCA(alignbuflen);
        for (UINT i = 0; i < BoolOption::getNumOfDumpOption(); i++) {
            UINT j = 0;
            for (; j < ::strlen(BoolOption::dump_option_desc[i].name); j++) {
                alignbuf[j] = BoolOption::dump_option_desc[i].name[j];
            }
            for (; j < alignbuflen - 1; j++) {
                alignbuf[j] = ' '; //pad the rest with blank
            }
            alignbuf[j] = 0;

            output.strcat(
                "\n -%s%s %s", BoolOption::dump_option_prefix,
                alignbuf, BoolOption::dump_option_desc[i].intro);
        }
    }
};

CHAR const* BoolOption::dump_option_prefix = "dump-";
CHAR const* BoolOption::disable_option_prefix = "no-";
CHAR const* BoolOption::only_option_prefix = "only-";
CHAR const* BoolOption::elf_option_prefix = "elf-";

BoolOption::Desc const BoolOption::option_desc[] = {
    { "time", &xoc::g_show_time,
      "show infer time", },
    //{ "licm", &xoc::g_do_licm,
    //  "enable loop-invariant-code-motion optimization", },
    //{ "licm_no_guard", &xoc::g_do_licm_no_guard,
    //  "enable loop-invariant-code-motion optimization "
    //  "without inserting loop guard", },
    //{ "if_cvs", &xoc::g_do_if_conversion,
    //  "enable if-conversion optimization", },
    //{ "gcse", &xoc::g_do_gcse,
    //  "enable global-common-subexpression-elimination optimization", },
    //{ "lcse", &xoc::g_do_lcse,
    //  "enable local-common-subexpression-elimination optimization", },
    //{ "exprtab", &xoc::g_do_expr_tab,
    //  "compute expression table", },
    //{ "rp", &xoc::g_do_rp,
    //  "enable register-promotion optimization", },
    //{ "cp", &xoc::g_do_cp,
    //  "enable copy-propagation optimization", },
    //{ "cp_aggr", &xoc::g_do_cp_aggressive,
    //  "enable aggressive copy-propagation optimization", },
    //{ "bcp", &xoc::g_do_bcp,
    //  "enable branch-condition-propagation optimization", },
    //{ "rce", &xoc::g_do_rce,
    //  "enable redundant-code-elimination optimization", },
    //{ "vect", &xoc::g_do_vect,
    //  "enable auto vectorization", },
    //{ "dce", &xoc::g_do_dce,
    //  "enable dead-code-elimination optimization", },
    //{ "dse", &xoc::g_do_dse,
    //  "enable dead-store-elimination optimization", },
    //{ "infer_type", &xoc::g_infer_type,
    //  "enable type inference", },
    //{ "vrp", &xoc::g_do_vrp,
    //  "enable value range propagation", },
    //{ "dce_aggr", &xoc::g_do_dce_aggressive,
    //  "enable aggressive-dead-code-elimination optimization", },
    //{ "lftr", &xoc::g_do_lftr,
    //  "enable linear-function-test-replacement optimization", },
    //{ "ivr", &xoc::g_do_ivr,
    //  "enable induction-variable-recognization", },
    //{ "gvn", &xoc::g_do_gvn,
    //  "enable global-value-numbering", },
    //{ "mdssa", &xoc::g_do_mdssa,
    //  "enable md-ssa analysis", },
    //{ "prssa", &xoc::g_do_prssa,
    //  "enable pr-ssa analysis", },
    //{ "reuse_ir", &xoc::g_reuse_ir,
    //  "reuse ir to reduce memory footprint", },
    //{ "prmode", &xoc::g_is_lower_to_pr_mode,
    //  "lowering cascading IR mode to pr-transfering mode", },
    //{ "lowest_height", &xoc::g_is_lower_to_lowest_height,
    //  "lowering cascading IR to the lowest height", },
    //{ "prdu", &xoc::g_compute_pr_du_chain,
    //  "enable classic PR def-use analysis", },
    //{ "nonprdu", &xoc::g_compute_nonpr_du_chain,
    //  "enable classic NON-PR def-use analysis", },
    //{ "fast_prdu", &xoc::g_compute_pr_du_chain_by_prssa,
    //  "enable classic fast PR def-use analysis", },
    //{ "redirect_stdout_to_dump_file", &xoc::g_redirect_stdout_to_dump_file,
    //  "redirect internal compiler output information to given dump file", },
    //{ "ipa", &xoc::g_do_ipa,
    //  "enable interprocedual analysis", },
    //{ "schedule_delay_slot", &xgen::g_enable_schedule_delay_slot,
    //  "enable scheduling branch-delay-slot", },
    //{ "cg", &xgen::g_do_cg,
    //  "enable target code generation", },
    //{ "gra", &xgen::g_do_gra,
    //  "enable global-register-allocation", },
    //{ "lis", &xgen::g_do_lis,
    //  "enable instruction-scheduling", },
    //{ "cg_for_inner_region", &xgen::g_is_generate_code_for_inner_region,
    //  "enable code generation for inner region", },
    //{ "refine", &xoc::g_do_refine,
    //  "enable refinement optimization", },
    //{ "global_refine", &xoc::g_do_global_refine,
    //  "enable global refinement optimization", },
    //{ "refine_duchain", &xoc::g_do_refine_duchain,
    //  "enable refine-duchain optimization", },
    //{ "lsra", &xoc::g_do_lsra,
    //  "enable linear-scan-register-allocation", },
    //{ "reass", &xoc::g_do_alge_reassociate,
    //  "enable algebraic-reasscociation", },
    //{ "reass_aggr", &xoc::g_do_alge_reassociate_aggressive,
    //  "enable aggressive-algebraic-reasscociation", },
    //{ "dist", &xoc::g_do_alge_distributive,
    //  "enable algebraic-distributive", },
    //{ "opt_float", &xoc::g_do_opt_float,
    //  "enable float optimization", },
    //#ifdef REF_TARGMACH_INFO
    //{ "migen", &mach::g_do_migen,
    //  "enable machine-instruction generation", },
    //#endif
    { "cont", &xinfer::g_continuation,
      "enable continuation", },
};


BoolOption::Desc const BoolOption::dump_option_desc[] = {
    //{ "before-pass", &xoc::g_dump_opt.is_dump_before_pass,
    //  "dump region before pass is performed", },
    //{ "after-pass", &xoc::g_dump_opt.is_dump_after_pass,
    //  "dump region after pass successed", },
    //{ "cfg", &xoc::DumpOption::getOptionDesc(PASS_CFG)->is_dump,
    //  "dump control-flow-graph", },
    //{ "cfgopt", &xoc::g_dump_opt.is_dump_cfgopt,
    //  "dump control-flow-graph optimizations", },
    //{ "rpo", &xoc::DumpOption::getOptionDesc(PASS_RPO)->is_dump,
    //  "dump PRO information", },
    //{ "dom", &xoc::DumpOption::getOptionDesc(PASS_DOM)->is_dump,
    //  "dump dominator information", },
    //{ "pdom", &xoc::DumpOption::getOptionDesc(PASS_PDOM)->is_dump,
    //  "dump post-dominator information", },
    //{ "aa", &xoc::DumpOption::getOptionDesc(PASS_AA)->is_dump,
    //  "dump alias-analysis", },
    //{ "dce", &xoc::DumpOption::getOptionDesc(PASS_DCE)->is_dump,
    //  "dump dead-code-elimination", },
    //{ "dse", &xoc::DumpOption::getOptionDesc(PASS_DSE)->is_dump,
    //  "dump dead-store-elimination", },
    //{ "infer_type", &xoc::DumpOption::getOptionDesc(PASS_INFER_TYPE)->is_dump,
    //  "dump infer-type", },
    //{ "vrp", &xoc::DumpOption::getOptionDesc(PASS_VRP)->is_dump,
    //  "dump value-range-propagation", },
    //{ "invert_brtgt",
    //  &xoc::DumpOption::getOptionDesc(PASS_INVERT_BRTGT)->is_dump,
    //  "dump invert-branch-target", },
    //{ "lftr", &xoc::DumpOption::getOptionDesc(PASS_LFTR)->is_dump,
    //  "dump linear-function-test-replacement optimization", },
    //{ "vect", &xoc::DumpOption::getOptionDesc(PASS_VECT)->is_dump,
    //  "dump loop vectorization", },
    //{ "ivr", &xoc::DumpOption::getOptionDesc(PASS_IVR)->is_dump,
    //  "dump induction-variable-recognization", },
    //{ "rp", &xoc::DumpOption::getOptionDesc(PASS_RP)->is_dump,
    //  "dump register-promotion", },
    //{ "licm", &xoc::DumpOption::getOptionDesc(PASS_LICM)->is_dump,
    //  "dump loop-invariant-code-motion", },
    //{ "if_cvs", &xoc::DumpOption::getOptionDesc(PASS_IF_CONVERSION)->is_dump,
    //  "dump if-conversion", },
    //{ "gcse", &xoc::DumpOption::getOptionDesc(PASS_GCSE)->is_dump,
    //  "dump global-common-subexpression-elimination", },
    //{ "lcse", &xoc::DumpOption::getOptionDesc(PASS_LCSE)->is_dump,
    //  "dump local-common-subexpression-elimination", },
    //{ "exprtab", &xoc::DumpOption::getOptionDesc(PASS_EXPR_TAB)->is_dump,
    //  "dump expression table", },
    //{ "dumgr", &xoc::DumpOption::getOptionDesc(PASS_DU_MGR)->is_dump,
    //  "dump classic def-use information", },
    //{ "prssa", &xoc::DumpOption::getOptionDesc(PASS_PRSSA_MGR)->is_dump,
    //  "dump pr-ssa information", },
    //{ "mdssa", &xoc::DumpOption::getOptionDesc(PASS_MDSSA_MGR)->is_dump,
    //  "dump md-ssa information", },
    //{ "rce", &xoc::DumpOption::getOptionDesc(PASS_RCE)->is_dump,
    //  "dump redundant-code-elimination", },
    //{ "bcp", &xoc::DumpOption::getOptionDesc(PASS_BCP)->is_dump,
    //  "dump redundant-code-elimination", },
    //{ "cp", &xoc::DumpOption::getOptionDesc(PASS_CP)->is_dump,
    //  "dump copy-propagation", },
    //{ "ir2or", &xgen::g_xgen_dump_opt.is_dump_ir2or,
    //  "dump IR2OR convertion", },
    //{ "ra", &xgen::g_xgen_dump_opt.is_dump_ra,
    //  "dump register allocation", },
    //{ "lis", &xgen::g_xgen_dump_opt.is_dump_lis,
    //  "dump instruction-scheduling", },
    //{ "cg", &xgen::g_xgen_dump_opt.is_dump_cg,
    //  "dump target code generation", },
    //{ "cdg", &xoc::DumpOption::getOptionDesc(PASS_CDG)->is_dump,
    //  "dump control-dependent-graph", },
    //{ "simplification", &xoc::DumpOption::getOptionDesc(PASS_IRSIMP)->is_dump,
    //  "dump IR simplification", },
    //{ "refine_duchain",
    //  &xoc::DumpOption::getOptionDesc(PASS_REFINE_DUCHAIN)->is_dump,
    //  "dump refine-duchain optimization", },
    //{ "refine", &xoc::DumpOption::getOptionDesc(PASS_REFINE)->is_dump,
    //  "dump refine optimization", },
    //{ "global_refine",
    //  &xoc::DumpOption::getOptionDesc(PASS_GLOBAL_REFINE)->is_dump,
    //  "dump global refine optimization", },
    //{ "gvn", &xoc::DumpOption::getOptionDesc(PASS_GVN)->is_dump,
    //  "dump global-value-numbering", },
    //{ "irparser", &xoc::g_dump_opt.is_dump_irparser,
    //  "dump IR parser", },
    //{ "all", &xoc::g_dump_opt.is_dump_all,
    //  "dump all compiler information", },
    //{ "for_test", &xoc::g_dump_opt.is_dump_for_test,
    //  "dump compiler information only for test environment usage", },
    //{ "irid", &xoc::g_dump_opt.is_dump_irid,
    //  "dump IR's id", },
    //{ "srcline", &xoc::g_dump_opt.is_dump_ir_srcline,
    //  "dump IR's src line", },
    //{ "attachinfo", &xoc::g_dump_opt.is_dump_ir_attachinfo,
    //  "dump IR's attachinfo", },
    //{ "lsra", &xoc::DumpOption::getOptionDesc(PASS_LINEAR_SCAN_RA)->is_dump,
    //  "dump linear-scan-register-allocation", },
    //{ "reass",
    //  &xoc::DumpOption::getOptionDesc(PASS_ALGE_REASSOCIATE)->is_dump,
    //  "dump algebraic-reasscociation", },
    //{ "dist",
    //  &xoc::DumpOption::getOptionDesc(PASS_ALGE_DISTRIBUTIVE)->is_dump,
    //  "dump algebraic-distributive", },
    //{ "to_buffer", &xoc::g_dump_opt.is_dump_to_buffer,
    //  "perfer to dump informations to buffer", },
    { "tensor_data", &xinfer::g_dump_tensor_data,
      "dump tensor data even if it might be slow when tensor is large", },
    { "json_parser", &xinfer::g_dump_json_parser, "dump json parser", },
    { "rt_norm", &xinfer::g_dump_rt_norm, "dump rt norm", },
    { "rt", &xinfer::g_dump_rt, "dump rt normal information", },
    { "rtctx", &xinfer::g_dump_rtctx, "dump rtctx", },
    { "rtarg", &xinfer::g_dump_rtarg, "dump rtarg", },
    { "rt_vec", &xinfer::g_dump_rt_vec, "dump rt vector", },
    { "act", &xinfer::g_dump_act, "dump action", },
    { "safetensors", &xinfer::g_dump_safetensors_mgr, "dump safetensors", },
};

BoolOption::Desc const BoolOption::elf_option_desc[] = {
    { "", nullptr, "", },
};


UINT BoolOption::getNumOfOption()
{
    return sizeof(BoolOption::option_desc) /
           sizeof(BoolOption::option_desc[0]);
}


UINT BoolOption::getNumOfDumpOption()
{
    return sizeof(BoolOption::dump_option_desc) /
           sizeof(BoolOption::dump_option_desc[0]);
}


UINT BoolOption::getNumOfELFOption()
{
    return sizeof(BoolOption::elf_option_desc) /
           sizeof(BoolOption::elf_option_desc[0]);
}

class FPOption {
public:
    class Desc {
    public:
        CHAR const* name;
        HOST_FP * option;
        CHAR const* intro;
    };
    static Desc const option_desc[];
    static UINT getNumOfOption();

    static bool is_option(CHAR const* str, HOST_FP ** option)
    {
        ASSERT0(option);
        for (UINT i = 0; i < FPOption::getNumOfOption(); i++) {
            if (::strcmp(FPOption::option_desc[i].name, str) == 0) {
                *option = FPOption::option_desc[i].option;
                return true;
            }
        }
        return false;
    }

    static void dump_usage(StrBuf & output)
    {
        UINT max_name_len = 0;
        for (UINT i = 0; i < FPOption::getNumOfOption(); i++) {
            max_name_len =
                MAX((UINT)::strlen(
                    FPOption::option_desc[i].name), max_name_len);
        }
        UINT alignbuflen = max_name_len + 2;

        CHAR * alignbuf = (CHAR*)ALLOCA(alignbuflen);
        for (UINT i = 0; i < FPOption::getNumOfOption(); i++) {
            UINT j = 0;
            for (; j < ::strlen(FPOption::option_desc[i].name); j++) {
                alignbuf[j] = FPOption::option_desc[i].name[j];
            }
            for (; j < alignbuflen - 1; j++) {
                alignbuf[j] = ' '; //pad the rest with blank
            }
            alignbuf[j] = 0;

            output.strcat(
                "\n -%s = <float> %s", alignbuf,
                FPOption::option_desc[i].intro);
        }
    }
};


FPOption::Desc const FPOption::option_desc[] = {
    { "temperature", (HOST_FP*)&xinfer::g_temperature,
      "scaling factor applied to logits, control randomness", },
};


UINT FPOption::getNumOfOption()
{
    return sizeof(FPOption::option_desc) /
           sizeof(FPOption::option_desc[0]);
}


class IntOption {
public:
    class Desc {
    public:
        CHAR const* name;
        INT * option;
        CHAR const* intro;
    };
    static Desc const option_desc[];
    static UINT getNumOfOption();

    static bool is_option(CHAR const* str, INT ** option)
    {
        ASSERT0(option);
        for (UINT i = 0; i < IntOption::getNumOfOption(); i++) {
            if (::strcmp(IntOption::option_desc[i].name, str) == 0) {
                *option = IntOption::option_desc[i].option;
                return true;
            }
        }
        return false;
    }

    static void dump_usage(StrBuf & output)
    {
        UINT max_name_len = 0;
        for (UINT i = 0; i < IntOption::getNumOfOption(); i++) {
            max_name_len =
                MAX((UINT)::strlen(
                    IntOption::option_desc[i].name), max_name_len);
        }
        UINT alignbuflen = max_name_len + 2;

        CHAR * alignbuf = (CHAR*)ALLOCA(alignbuflen);
        for (UINT i = 0; i < IntOption::getNumOfOption(); i++) {
            UINT j = 0;
            for (; j < ::strlen(IntOption::option_desc[i].name); j++) {
                alignbuf[j] = IntOption::option_desc[i].name[j];
            }
            for (; j < alignbuflen - 1; j++) {
                alignbuf[j] = ' '; //pad the rest with blank
            }
            alignbuf[j] = 0;

            output.strcat(
                "\n -%s = <integer> %s", alignbuf,
                IntOption::option_desc[i].intro);
        }
    }
};


IntOption::Desc const IntOption::option_desc[] = {
    //{ "thres_opt_ir_num", (INT*)&xoc::g_thres_opt_ir_num,
    //  "the maximum limit of the number of IR to perform optimizations", },
    //{ "thres_opt_bb_num", (INT*)&xoc::g_thres_opt_bb_num,
    //  "the maximum limit of the number of IRBB to perform optimizations", }
    { "max_tensor_data_dump_num", (INT*)&xinfer::g_max_tensor_data_dump_num,
      "the maximum limit of the number of tensor data to dump", },
    { "max_seq_len", (INT*)&xinfer::g_max_seq_len,
      "the maximum limit of the number of sequence length", },
    { "topk", (INT*)&xinfer::g_topk,
      "topk setting, adjust the top-k count to control the token number. "
      "If it equals 1, sampling falls back to argmax", },
};


UINT IntOption::getNumOfOption()
{
    return sizeof(IntOption::option_desc) /
           sizeof(IntOption::option_desc[0]);
}


class StringOption {
public:
    class Desc {
    public:
        CHAR const* name;
        CHAR * option;
        CHAR const* intro;
    };
    static Desc const option_desc[];
    static UINT getNumOfOption();

    static bool is_option(CHAR const* str, CHAR const** option)
    {
        ASSERT0(option);
        for (UINT i = 0; i < StringOption::getNumOfOption(); i++) {
            if (::strcmp(StringOption::option_desc[i].name, str) == 0) {
                *option = StringOption::option_desc[i].option;
                return true;
            }
        }
        return false;
    }

    static void dump_usage(StrBuf & output)
    {
        UINT max_name_len = 0;
        for (UINT i = 0; i < StringOption::getNumOfOption(); i++) {
            max_name_len = MAX((UINT)::strlen(
                StringOption::option_desc[i].name), max_name_len);
        }
        UINT alignbuflen = max_name_len + 2;

        CHAR * alignbuf = (CHAR*)ALLOCA(alignbuflen);
        for (UINT i = 0; i < StringOption::getNumOfOption(); i++) {
            UINT j = 0;
            for (; j < ::strlen(StringOption::option_desc[i].name); j++) {
                alignbuf[j] = StringOption::option_desc[i].name[j];
            }
            for (; j < alignbuflen - 1; j++) {
                alignbuf[j] = ' '; //pad the rest with blank
            }
            alignbuf[j] = 0;

            output.strcat(
                "\n -%s = \"string1,string2,string3\" %s", alignbuf,
                StringOption::option_desc[i].intro);
        }
    }
};


StringOption::Desc const StringOption::option_desc[] = {
    //{ "include_region", (CHAR*)&xinfer::g_include_string,
    //   "the list of region that participate in compilation", },
    //{ "exclude_region", (CHAR*)&xinfer::g_exclude_string,
    //   "the list of region that will be excluded from compilation", },
    { "prompt", (CHAR*)&xinfer::g_prompt,
       "the prompt that user input", },
    { "model_name", (CHAR*)&xinfer::g_model_name,
       "the model used", },
};


UINT StringOption::getNumOfOption()
{
    return sizeof(StringOption::option_desc) /
           sizeof(StringOption::option_desc[0]);
}


static bool dispatchByPrefixDump(
    CHAR const* cmdstr, INT argc, CHAR const* argv[],
    INT & i, OUT OptWrap & ow)
{
    if (BoolOption::is_dump_option(cmdstr, ow)) {
        ASSERT0(ow.boption_ptr);
        **ow.boption_ptr = true;
        i++;
        inferDumpOption(cmdstr);
        return true;
    }
    return false;
}


static bool dispatchByPrefixELF(
    CHAR const* cmdstr, INT argc, CHAR const* argv[],
    INT & i, OUT OptWrap & ow)
{
    if (BoolOption::is_elf_option(cmdstr, ow)) {
        ASSERT0(ow.boption_ptr);
        **ow.boption_ptr = true;
        i++;
        return true;
    }
    return false;
}


static bool recogOption(
    CHAR const* cmdstr, INT argc, CHAR const* argv[], INT & i,
    OUT OptWrap & ow)
{
    if (BoolOption::is_option(cmdstr, ow)) {
        ASSERT0(*ow.boption_ptr);
        **ow.boption_ptr = true;
        i++;
        return true;
    }

    INT * int_option = nullptr;
    if (IntOption::is_option(cmdstr, &int_option)) {
        ASSERT0(int_option);
        CHAR const* n = nullptr;
        if (i + 1 < argc && argv[i + 1] != nullptr) {
            n = argv[i + 1];
        }
        if (n == nullptr) { return false; }
        if (::strcmp(n, "=") != 0) { return false; }
        if (i + 2 < argc && argv[i + 2] != nullptr) {
            n = argv[i + 2];
        }
        if (n == nullptr) { return false; }
        *int_option = (INT)xcom::xatoll(n, false);
        i += 3;
        return true;
    }

    HOST_FP * fp_option = nullptr;
    if (FPOption::is_option(cmdstr, &fp_option)) {
        ASSERT0(fp_option);
        CHAR const* n = nullptr;
        if (i + 1 < argc && argv[i + 1] != nullptr) {
            n = argv[i + 1];
        }
        if (n == nullptr) { return false; }
        if (::strcmp(n, "=") != 0) { return false; }
        if (i + 2 < argc && argv[i + 2] != nullptr) {
            n = argv[i + 2];
        }
        if (n == nullptr) { return false; }
        *fp_option = (HOST_FP)::atof(n);
        i += 3;
        return true;
    }

    CHAR const* str_option = nullptr;
    if (StringOption::is_option(cmdstr, &str_option)) {
        ASSERT0(str_option);
        CHAR const* n = nullptr;
        if (i + 1 < argc && argv[i + 1] != nullptr) {
            n = argv[i + 1];
        }
        if (n == nullptr) { return false; }
        if (::strcmp(n, "=") != 0) { return false; }
        if (i + 2 < argc && argv[i + 2] != nullptr) {
            n = argv[i + 2];
        }
        if (n == nullptr) { return false; }
        *(CHAR const**)str_option = n;
        i += 3;
        inferStringOption();
        return true;
    }
    return false;
}


static bool tryDispatchCreateDump(
    CHAR const* cmdstr, INT argc, CHAR const* argv[], INT & i)
{
    if (::strcmp(cmdstr, "dump") == 0) {
        CHAR const* n = process_dump(argc, argv, i);
        if (n == nullptr) {
            return false;
        }
        g_dump_file_name = n;
        return true;
    }
    return false;
}


static bool tryDispatchHelp(
    CHAR const* cmdstr, INT argc, CHAR const* argv[], INT & i)
{
    if (::strcmp(cmdstr, "help") == 0 || ::strcmp(cmdstr, "h") == 0) {
        g_ask_for_help = true;
        return true;
    }
    return false;
}


//No prefix.
static bool tryDispatchNonPrefixOption(
    CHAR const* cmdstr, INT argc, CHAR const* argv[], INT & i,
    OUT OptWrap & ow)
{
    ASSERT0(ow.boption_ptr);
    return recogOption(cmdstr, argc, argv, i, ow);
}


static bool tryDispatchELFOption(
    CHAR const* cmdstr, INT argc, CHAR const* argv[], INT & i,
    OUT OptWrap & ow)
{
    CHAR const* prefix = BoolOption::elf_option_prefix;
    if (xcom::xstrcmp(cmdstr, prefix, (INT)strlen(prefix))) {
        return dispatchByPrefixELF(
            cmdstr + strlen(prefix), argc, argv, i, ow);
    }
    return false;
}


static bool tryDispatchOnlyOption(
    CHAR const* cmdstr, INT argc, CHAR const* argv[], INT & i,
    OUT OptWrap & ow)
{
    CHAR const* prefix = BoolOption::only_option_prefix;
    if (xcom::xstrcmp(cmdstr, prefix, (INT)::strlen(prefix))) {
        ASSERT0(ow.boption_ptr);
        bool res = dispatchByPrefix(
            cmdstr + ::strlen(prefix), argc, argv, i, ow);
        if (!res) { return res; }
        ASSERT0(ow.option_kind != OPT_KIND_UNDEF);
        if (ow.option_kind == OPT_KIND_FUNC) {
            disableAllOpt(xoc::g_opt_level);
        } else {
            ASSERT0(ow.option_kind == OPT_KIND_DUMP);
            disableAllDumpOption();
        }
        ASSERT0(*ow.boption_ptr);
        **ow.boption_ptr = true;
        return res;
    }
    return false;
}


static bool tryDispatchDumpOption(
    CHAR const* cmdstr, INT argc, CHAR const* argv[], INT & i,
    OUT OptWrap & ow)
{
    CHAR const* prefix = BoolOption::dump_option_prefix;
    if (xcom::xstrcmp(cmdstr, prefix, (INT)::strlen(prefix))) {
        return dispatchByPrefixDump(
            cmdstr + ::strlen(prefix), argc, argv, i, ow);
    }
    return false;
}


static bool tryDispatchDisable(
    CHAR const* cmdstr, INT argc, CHAR const* argv[], INT & i,
    OUT OptWrap & ow)
{
    CHAR const* prefix = BoolOption::disable_option_prefix;
    if (xcom::xstrcmp(cmdstr, prefix, (INT)::strlen(prefix))) {
        ASSERT0(ow.boption_ptr);
        bool res = dispatchByPrefix(
            cmdstr + ::strlen(prefix), argc, argv, i, ow);
        if (!res) { return res; }
        ASSERT0(*ow.boption_ptr);
        //**ow.boption_ptr = !(**ow.boption_ptr);
        **ow.boption_ptr = false;
        return res;
    }
    return false;
}


static bool tryDispatchOptAndOutput(
    CHAR const* cmdstr, INT argc, CHAR const* argv[], INT & i)
{
    switch (cmdstr[0]) {
    case 'O':
        if (xcom::xisdigit(cmdstr[1])) {
            return process_opt(argc, argv, i);
        }
        break;
    case 'o':
        if (cmdstr[1] == 0) {
            return process_output_file(argc, argv, i);
        }
        break;
    default:;
    }
    return false;
}


//The function dispatchs the processing of cmdline recursively by
//prefix string.
static bool dispatchByPrefix(
    CHAR const* cmdstr, INT argc, CHAR const* argv[], INT & i,
    OUT OptWrap & ow)
{
    if (tryDispatchHelp(cmdstr, argc, argv, i)) {
        return true;
    }
    if (tryDispatchOptAndOutput(cmdstr, argc, argv, i)) {
        return true;
    }
    if (tryDispatchCreateDump(cmdstr, argc, argv, i)) {
        return true;
    }
    if (tryDispatchDisable(cmdstr, argc, argv, i, ow)) {
        return true;
    }
    if (tryDispatchDumpOption(cmdstr, argc, argv, i, ow)) {
        return true;
    }
    if (tryDispatchOnlyOption(cmdstr, argc, argv, i, ow)) {
        return true;
    }
    if (tryDispatchELFOption(cmdstr, argc, argv, i, ow)) {
        return true;
    }
    if (tryDispatchNonPrefixOption(cmdstr, argc, argv, i, ow)) {
        return true;
    }
    return false;
}


static void usage()
{
    StrBuf buf1(128);
    StrBuf buf2(128);
    StrBuf buf3(128);
    StrBuf buf4(128);
    BoolOption::dump_usage(buf1);
    IntOption::dump_usage(buf2);
    StringOption::dump_usage(buf3);
    BoolOption::dump_dump_option(buf4);

    fprintf(stdout,
    "\nxinfer Version %s"
    "\nUsage: xinfer [options] file"
    "\noptions: "
    "\n -h,              show command line usage"
    "\n -help,           show command line usage"
    //"\n -O0,             compile without any optimization"
    //"\n -O1,-O2,-O3      compile with optimizations"
    "\n -dump <file>     create dump file"
    "\n -o <file>        output file name"
    "\n -prompt <string> prompt information"
    //"\n -readgr <file>   read GR file"
    "\n -no-<option>     disable option, e.g:-no-dce"
    "\n -dump-<option>   dump information about option, e.g:-dump-dce"
    "\n"
    "\noption: ",
    g_version);

    StrBuf buf(128);
    BoolOption::dump_usage(buf);
    fprintf(stdout, "%s", buf.getBuf());

    IntOption::dump_usage(buf);
    fprintf(stdout, "%s", buf.getBuf());

    StringOption::dump_usage(buf);
    fprintf(stdout, "%s", buf.getBuf());

    fprintf(stdout, "\n");

    BoolOption::dump_dump_option(buf);
    fprintf(stdout, "\navailable dump option: %s", buf.getBuf());

    fprintf(stdout, "\n");
}


static void forceOption()
{
    //Enable infer-type even if it is disabled by cmdline when CG is enabled.
    xoc::g_infer_type = true;
}


static void inferDumpOption(CHAR const* cmdstr)
{
    if (::strcmp(cmdstr, "all") == 0) {
        xoc::g_dump_opt.setDumpAll();

        //Set XGEN option according to XOC option.
        xgen::g_xgen_dump_opt.is_dump_all = xoc::g_dump_opt.isDumpAll();
        return;
    }
    if (::strcmp(cmdstr, "nothing") == 0) {
        xoc::g_dump_opt.setDumpNothing();
        return;
    }
}


static void disableAllDumpOption()
{
    inferDumpOption("nothing");
}


static void inferStringOption()
{
    if (g_include_string != nullptr) {
        xoc::g_include_region.addString(g_include_string);
    }
    if (g_exclude_string != nullptr) {
        xoc::g_exclude_region.addString(g_exclude_string);
    }
}


//The position of unknown option in 'argv'.
static void report_unknown_option(UINT pos, CHAR const* argv[])
{
    fprintf(stdout, "\n");
    for (UINT i = 0; i <= pos; i++) {
        fprintf(stdout, "%s ", argv[i]);
    }
    fprintf(stdout, "\nunknown option:%s\n", argv[pos]);
}


bool processCmdLine(INT argc, CHAR const* argv[])
{
    if (argc <= 1) { usage(); return false; }

    //TBD:Does insert CVT is needed?
    xoc::g_insert_cvt = true;
    xoc::g_do_refine = true;

    //Retain CFG, DU info for IPA used.
    xoc::g_compute_region_imported_defuse_md = true;

    //Post-simplification need IRMgr and PassMgr.
    //xoc::g_retain_pass_mgr_for_region = false;
    //xoc::g_compute_pr_du_chain = false;
    //xoc::g_compute_nonpr_du_chain = false;
    //xoc::g_do_call_graph = true;
    //xoc::g_do_ipa = true;
    xoc::g_is_support_dynamic_type = true;
    xoc::g_do_opt_float = false;
    for (INT i = 1; i < argc;) {
        if (argv[i][0] == '-') {
            OptWrap ow;
            if (!dispatchByPrefix(&argv[i][1], argc, argv, i, ow)) {
                report_unknown_option(i, argv);
                return false;
            }
            if (g_ask_for_help) {
                usage();
                return false;
            }
            continue;
        }
        if (is_safetensors_source_file(argv[i])) {
            g_safetensors_file_list.append_tail(argv[i]);
            i++;
            continue;
        }
        if (is_gguf_source_file(argv[i])) {
            g_gguf_file_list.append_tail(argv[i]);
            i++;
            continue;
        }
        if (is_json_file(argv[i])) {
            g_json_file_list.append_tail(argv[i]);
            i++;
            continue;
        }
        if (is_txt_file(argv[i])) {
            g_txt_file_list.append_tail(argv[i]);
            i++;
            continue;
        }
        report_unknown_option(i, argv);
        return false;
    }
    forceOption();
    return true;
}

} //namespace xinfer
