/*@
XOC Release License

Copyright (c) 2013-2014, Alibaba Group, All rights reserved.

    compiler@aliexpress.com

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

THIS SOFTWARE IS PROVIDED "AS IS" AND ANY EXPRESS OR IMPLIED
WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS
BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT
OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN
IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

author: Su Zhenyu
@*/
#include "libdex/DexFile.h"
#include "libdex/DexClass.h"
#include "libdex/DexDebugInfo.h"
#include "liropcode.h"
#include "lir.h"
#include "d2d_comm.h"
#include "d2d_l2d.h"
#include "drAlloc.h"
#include "cominc.h"
#include "comopt.h"
#include "dex.h"
#include "trycatch_info.h"
#include "gra.h"
#include "dex_hook.h"
#include "dex_util.h"
#include "dex2ir.h"
#include "ir2dex.h"
#include "dex_driver.h"

//Ensure RETURN IR at the end of function if its return-type is VOID.
static void add_ret(IR * irs, Region * rg)
{
    IR * last = get_last(irs);
    if (last != nullptr && !last->is_return()) {
        IR * ret = rg->buildReturn(nullptr);
        if (irs == nullptr) {
            irs = ret;
        } else {
            insertafter_one(&last, ret);
        }
    }
}


//Upate try-catch info if any.
static void updateLIRCodeOrg(LIRCode * lircode, Dex2IR & d2ir, IR2Dex & ir2d)
{
    TryInfo * ti = d2ir.getTryInfo();
    Label2UINT * lab2idx = ir2d.get_lab2idx();
    if (ti == nullptr) { return; }

    ASSERT0(lircode->trys);
    UINT i = 0;
    for (; ti != nullptr; ti = ti->next, i++) {
        bool find;
        UINT idx = lab2idx->get(ti->try_start, &find);
            ASSERT0(find);
            LIROpcodeTry * each_try = lircode->trys + i;
            each_try->start_pc = idx;

            idx = lab2idx->get(ti->try_end, &find);
            ASSERT0(find);

            /*
            The means of value of try end position is not
            same as try start position.
            e.g:
                0th: iget
                1th: add
                2th: move
                iget and add belong to try block, and move is not.
                try-start is 0th, try-end is 2th.
                So the last try-end position (if has) without lir
                corresponding to.
            */
            each_try->end_pc = idx;
            //ASSERT0(each_try->end_pc > each_try->start_pc);

            UINT j = 0;
            for (CatchInfo * ci = ti->catch_list;
                 ci != nullptr; ci = ci->next, j++) {
                ASSERT0(each_try->catches);
                LIROpcodeCatch * each_catch = each_try->catches + j;

                UINT idx = lab2idx->get(ci->catch_start, &find);
                ASSERT0(find);
                each_catch->handler_pc = idx;
                each_catch->class_type = ci->kind;
            }

        if (ti->catch_list != nullptr) {
            ASSERT0(j == each_try->catchSize);
        }
    }
    ASSERT0(i == lircode->triesSize);
}


//Update try-catch info if any.
static void updateTryInfo(LIRCode * lircode, Dex2IR & d2ir, IR2Dex & ir2d)
{
    TryInfo * ti = d2ir.getTryInfo();
    if (ti == nullptr) { return; }

    Label2UINT * lab2idx = ir2d.get_lab2idx();
    Vector<TryInfo*> tivec;

    //Check if catch blocks has removed, delete the tryinfo.
    UINT vecidx = 0;
    for (UINT i = 0; ti != nullptr; ti = ti->next, i++) {
        //Check if try-block is not exist.
        bool find_start_lab = false;
        bool find_end_lab = false;
        lab2idx->get(ti->try_start, &find_start_lab);
        lab2idx->get(ti->try_end, &find_end_lab);
        if (!find_start_lab || !find_end_lab) {
            if (!find_end_lab &&
                d2ir.getLastTryEndLabelList().find(ti->try_end)) {
                //ti->try_end label will be added the end of LIR list.
            } else if (!find_start_lab &&
                       d2ir.getLastTryEndLabelList().find(ti->try_start)) {
                //Could this happen?
                //ti->try_start label will be added the end of LIR list.
            } else {
                ASSERTN(!(find_start_lab ^ find_end_lab),
                       ("try_start and try_end must both exist or not"));
                continue; //current try-block has been removed, just neglect it.
            }
        }

        //Check if catch-block is not exist.
        UINT j = 0;
        LIROpcodeTry * each_try = &lircode->trys[i];

        bool at_least_one_catch_block = false;
        for (CatchInfo * ci = ti->catch_list;
             ci != nullptr; ci = ci->next, j++) {
            ASSERT0(each_try->catches);
            ASSERT0(j < each_try->catchSize);
            LIROpcodeCatch * each_catch = &each_try->catches[j];

            bool find;
            UINT idx = lab2idx->get(ci->catch_start, &find);
            if (find) {
                at_least_one_catch_block = true;
                break;
            }
        }

        //The try-block is available for generating.
        if (at_least_one_catch_block) {
            tivec.set(vecidx, ti);
            vecidx++;
        }
    }

    //Regenerate try-block and catch-block informations.
    ASSERT0(lircode->trys);
    VecIdx i = 0;
    for (; i <= tivec.get_last_idx(); i++) {
        TryInfo * ti = tivec[i];
        ASSERT0(ti);

        bool find;
        UINT idx = lab2idx->get(ti->try_start, &find);
        ASSERT0(find);
        LIROpcodeTry * each_try = &lircode->trys[i];
        each_try->start_pc = idx;

        idx = lab2idx->get(ti->try_end, &find);
        if (!find) {
            ASSERTN(d2ir.getLastTryEndLabelList().find(ti->try_end),
                   ("Miss the try-end label, it may be deleted incorrectly."));
            idx = lircode->instrCount;
        }

        //The means of value of try_end position is not
        //same as try start position.
        //e.g:
        //    0th: iget
        //    1th: add
        //    2th: move
        //    iget and add belong to try block, and move is not.
        //    try-start is 0th, try-end is 2th.
        //    So the last try-end position (if has) without lir
        //    corresponding to.
        each_try->end_pc = idx;
        //ASSERT0(each_try->end_pc > each_try->start_pc);

        ASSERTN(each_try->catches, ("miss catch-block field"));

        //Generate catch-block info.
        UINT j = 0;
        for (CatchInfo * ci = ti->catch_list; ci != nullptr; ci = ci->next) {
            LIROpcodeCatch * each_catch = &each_try->catches[j];

            UINT idx = lab2idx->get(ci->catch_start, &find);
            if (find) {
                each_catch->handler_pc = idx;
                each_catch->class_type = ci->kind;
                j++;
            }
        }

        if (ti->catch_list != nullptr) {
            ASSERT0(j > 0);
            each_try->catchSize = j;
        }
    }

    lircode->triesSize = i;
}


static void do_opt(IR * ir_list, DexRegion * func_ru)
{
    if (ir_list == nullptr) { return; }

    //dumpIRList(ir_list, func_ru->getTypeMgr());

    bool change;

    RefineCtx rf(&oc);

    //Do not insert cvt for DEX code to avoid smash the code sanity.
    RC_insert_cvt(rf) = false;

    //ir_list = func_ru->refineIRlist(ir_list, change, rf);

    verifyIRList(ir_list, nullptr, func_ru);

    func_ru->addToIRList(ir_list);

    #if 1
    OptCtx oc;
    bool succ = func_ru->process(&oc);
    ASSERT0(succ);
    #else
    func_ru->processSimply();
    #endif

    func_ru->addToIRList(func_ru->constructIRlist(true));
}


//Convert LIR code to DEX code and store it to fupool code buffer.
//df: dex file handler.
//dexm: record the dex-method information.
//fu: record the LIR code buffer and related information.
//fupool: record the transformed DEX code.
static void convertLIR2Dex(
        IN DexFile * df,
        DexMethod const* dexm,
        IN LIRCode * lircode,
        OUT D2Dpool * fupool)
{
    DexCode const* dexcode = dexGetCode(df, dexm);
    DexCode x; //only for local used.
    ::memset((void*)&x, 0, sizeof(DexCode));

    //Transform LIR to DEX.
    CBSHandle transformed_dex_code = transformCode(lircode, &x);

    //Record in fupool.
    DexCode * newone = writeCodeItem(fupool,
                                    transformed_dex_code,
                                    x.registersSize,
                                    x.insSize,
                                    dexcode->outsSize,
                                    x.triesSize,
                                    dexcode->debugInfoOff,
                                    x.insnsSize);

    #if 0
    //#ifdef _DEBUG_
    //For dump purpose.
    AocDxMgr adm(df);
    DX_INFO dxinfo;
    adm.extract_dxinfo(dxinfo,
                       newone->insns,
                       newone->insnsSize,
                       nullptr, &dexm->methodIdx);
    adm.dump_method(dxinfo, g_tfile);
    #endif
}


//Convert IR to LIR code. Store the generated IR list
//at lircode buffer.
static void convertIR2LIR(
        IN DexRegion * func_ru,
        IN DexFile * df,
        OUT LIRCode * lircode)
{
    IR2Dex ir2dex(func_ru, df);
    List<LIR*> newlirs;
    ir2dex.convert(func_ru->getIRList(), newlirs);

    //to LIRCode.
    UINT u = newlirs.get_elem_count();
    lircode->instrCount = u;
    lircode->lirList = (LIR**)LIRMALLOC(u * sizeof(LIR*));
    lircode->maxVars = func_ru->getPrno2Vreg()->maxreg + 1;
    ASSERT0(lircode->numArgs == func_ru->getPrno2Vreg()->paramnum);
    ::memset((void*)lircode->lirList, 0, u * sizeof(LIR*));
    UINT i = 0;
    for (LIR * l = newlirs.get_head(); l != nullptr; l = newlirs.get_next()) {
        ASSERT0(l);
        lircode->lirList[i++] = l;
    }
    updateTryInfo(lircode, *func_ru->getDex2IR(), ir2dex);
    //updateLIRCodeOrg(lircode, *func_ru->getDex2IR(), ir2dex);
}


class DbxCtx {
public:
    DexRegion * region;
    OffsetVec const& offsetvec;
    UINT last_instruction_index;
    DbxVec & dbxvec;
    SMemPool * pool;
    UINT lircode_num;
    DexDbx * last_debug_info;

public:
    DbxCtx(DexRegion * rg,
           OffsetVec const& off,
           OUT DbxVec & dv,
           SMemPool * p,
           UINT n) :
        region(rg),
        offsetvec(off),
        last_instruction_index(0),
        dbxvec(dv),
        pool(p),
        lircode_num(n),
        last_debug_info(nullptr)
    { ASSERT0(rg); }

    DexDbx * newDexDbx()
    {
        ASSERT0(pool);
        DexDbx * dd = (DexDbx*)smpoolMalloc(sizeof(DexDbx), pool);
        ASSERT0(dd);
        ::memset((void*)dd, 0, sizeof(DexDbx));
        dd->init(AI_DBX);
        return dd;
    }
};


//Callback for "new position table entry".
//Returning non-0 causes the decoder to stop early.
int dumpPosition(void * cnxt, u4 dexpc, u4 linenum)
{
    DbxCtx * ctx = (DbxCtx*)cnxt;

    //Generate new debug info.
    DexDbx * newdbx = ctx->newDexDbx();
    newdbx->linenum = linenum;
    newdbx->filename = ctx->region->getClassSourceFilePath();

    //Get the last debug info generated.
    DexDbx * lastdbx = ctx->last_debug_info;

    //Assign the subsequently LIR from last recording-point
    //with the last debug info.
    UINT i = ctx->last_instruction_index;
    for (; i < ctx->lircode_num && ctx->offsetvec[i] < dexpc; i++) {
        ctx->dbxvec[i] = lastdbx;
    }

    //Record point, to record the debug info for followed lir used.
    ctx->last_instruction_index = i;
    ctx->last_debug_info = newdbx;
    return 0;
}


//Callback for "new locals table entry". "signature" is an empty string
//if no signature is available for an entry.
void dumpLocal(
        void *cnxt,
        u2 reg,
        u4 startAddress,
        u4 endAddress,
        const char *name,
        const char *descriptor,
        const char *signature)
{
    DbxCtx * dc = (DbxCtx*)cnxt;

    //note("\nstartAddr:0x%04x, endAddr:0x%04x, name:%s, desc:%s, signature:%s",
    //            startAddress,
    //            endAddress,
    //            name,
    //            descriptor,
    //            signature);
    //fflush(g_tfile);
}


//Callback for "new locals table entry". "signature" is an empty string
//if no signature is available for an entry.
static void parseDebugInfo(
        DexRegion * rg,
        DexFile const* df,
        DexCode const* dexcode,
        DexMethod const* dexmethod,
        LIRCode const* lircode,
        OffsetVec const& offsetvec,
        SMemPool * pool,
        OUT DbxVec & dbxvec)
{
    DexMethodId const* methodid = dexGetMethodId(df, dexmethod->methodIdx);

    DbxCtx dc(rg, offsetvec, dbxvec, pool, LIRC_num_of_op(lircode));

    dexDecodeDebugInfo(
            df,
            dexcode,
            get_class_name(df, dexmethod),
            methodid->protoIdx,
            dexmethod->accessFlags,
            dumpPosition,
            dumpLocal,
            &dc);

    //Assign the final LIRs from last recording-point
    //with the last debug info.
    UINT i = dc.last_instruction_index;
    //Get the last debug info generated.
    DexDbx * lastdbx = dc.last_debug_info;
    for (; i < dc.lircode_num; i++) {
        dc.dbxvec[i] = lastdbx;
    }
}


static void handleRegion(
        IN DexRegion * func_ru,
        IN DexFile * df,
        MOD LIRCode * lircode,
        D2Dpool * fupool,
        DexMethod const* dexm,
        DexCode const* dexcode,
        OffsetVec const& offsetvec)
{
    DbxVec dbxvec(LIRC_num_of_op(lircode));

    SMemPool * dbxpool = nullptr; //record the all DexDbx data.
    if (g_collect_debuginfo) {
        if (g_do_ipa) {
            dbxpool = ((DexRegionMgr*)func_ru->getRegionMgr())->get_pool();
        } else {
            dbxpool = smpoolCreate(sizeof(DexDbx), MEM_COMM);
        }

        parseDebugInfo(func_ru, df, dexcode, dexm, lircode,
                       offsetvec, dbxpool, dbxvec);
    }

    TypeIndexRep tr;
    TypeMgr * dm = func_ru->getTypeMgr();
    tr.i8 = dm->getSimplexType(D_I8);
    tr.u8 = dm->getSimplexType(D_U8);
    tr.i16 = dm->getSimplexType(D_I16);
    tr.u16 = dm->getSimplexType(D_U16);
    tr.i32 = dm->getSimplexType(D_I32);
    tr.u32 = dm->getSimplexType(D_U32);
    tr.i64 = dm->getSimplexType(D_I64);
    tr.u64 = dm->getSimplexType(D_U64);
    tr.f32 = dm->getSimplexType(D_F32);
    tr.f64 = dm->getSimplexType(D_F64);
    tr.b = dm->getSimplexType(D_B);
    tr.ptr = dm->getPointerType(OBJ_MC_SIZE);
    tr.array = dm->getPointerType(ARRAY_MC_SIZE);
    func_ru->setTypeIndexRep(&tr);

    Dex2IR d2ir(func_ru, df, lircode, dbxvec);
    bool succ;
    IR * ir_list = d2ir.convert(&succ);

    //Map that records the Prno to Vreg mapping.
    Prno2Vreg prno2v(getNearestPowerOf2(
            d2ir.getPR2Vreg()->get_elem_count() + 1));

    if (!succ) {
        goto FIN;
    }

    func_ru->setDex2IR(&d2ir);

    if (d2ir.hasCatch()) {
        //goto FIN;
    }

    //dumpIRList(ir_list, func_ru->getTypeMgr());
    func_ru->setPrno2Vreg(&prno2v);

    #if 1
    do_opt(ir_list, func_ru);
    #else
    LOG("\t\tdo pass test: '%s'", func_ru->getRegionName());
    func_ru->addToIRList(ir_list);
    func_ru->getPrno2Vreg()->clean();
    func_ru->getPrno2Vreg()->copy(*func_ru->getDex2IR()->getPR2Vreg());
    #endif

    if (!g_retain_pass_mgr_for_region) {
        //TODO: support convert LIR from IRBB list.
        convertIR2LIR(func_ru, df, lircode);
    }
FIN:
    if (dbxpool != nullptr && !g_do_ipa) {
        smpoolDelete(dbxpool); //delete the pool local used.
    }
    return;
}


int debug2()
{
    return 1;
}


static int pcount = 0;
int xdebug()
{
    pcount++;
    return 1;
}


void logd_fu_param(CHAR const* runame, LIRCode * fu)
{
    CHAR * buf = (CHAR*)ALLOCA(fu->numArgs * 10 + 100 + strlen(runame));
    sprintf(buf, "\nxoc copmile:%s, count=%d maxreg:%d ", runame, pcount, fu->maxVars - 1);
    CHAR * p = buf + strlen(buf);
    if (fu->maxVars > 0) {
        strcat(p, "(");
        p += strlen(p);
        for (INT i = fu->maxVars - fu->numArgs; i < (INT)fu->maxVars; i++) {
            ASSERT0(i >= 0);
            if (i != (INT)fu->maxVars - 1) {
                sprintf(p, "v%d,", i);
            } else {
                sprintf(p, "v%d", i);
            }
            p += strlen(p);
        }
        strcat(p, ")");
    }
    LOG("%s ", buf);
}


bool g_dd = false;
bool is_compile(CHAR const* runame, LIRCode * fu)
{
    xdebug();
    logd_fu_param(runame, fu);
    g_dd = 0; //set to 1 to open dex2ir.log ir2dex.log
    return true;
}


static void dump_all_method_name(CHAR const* runame)
{
    FILE * h = fopen("method.log", "a+");
    fprintf(h, "\n%s", runame);
    fclose(h);
}


//Optimizer for LIR.
//Return true if compilation is successful.
bool compileFunc(
        MOD RegionMgr * rumgr,
        OUT D2Dpool * fupool,
        IN LIRCode * lircode,
        IN DexFile * df,
        DexMethod const* dexm,
        DexCode const* dexcode,
        DexClassDef const* dexclassdef,
        OffsetVec const& offsetvec,
        List<DexRegion const*> * rulist)
{
    CHAR tmp[256];
    CHAR * runame = nullptr;
    CHAR const* classname = get_class_name(df, dexm);
    CHAR const* funcname = get_func_name(df, dexm);
    CHAR const* functype = get_func_type(df, dexm);
    UINT len = strlen(classname) + strlen(funcname) + strlen(functype) + 10;

    if (len < 256) { runame = tmp; }
    else {
        runame = (CHAR*)ALLOCA(len);
        ASSERT0(runame);
    }

    //Function string is consist of these.
    assemblyUniqueName(runame, classname, functype, funcname);


    g_dump_ir2dex = false;
    g_dump_dex2ir = false;
    g_dump_classdefs = false;
    g_dump_lirs = false;

    g_opt_level = OPT_LEVEL3;

    ASSERT0(df && dexm);

    if (g_dump_classdefs) {
        dump_all_class_and_field(df);
    }

    if (g_dump_lirs) {
        dump_all_lir(lircode, df, dexm);
    }

    DexRegionMgr * rm = nullptr;
    if (g_do_ipa) {
        ASSERT0(rumgr);
        rm = (DexRegionMgr*)rumgr;
    } else {
        ASSERT0(rumgr == nullptr);
        rm = new DexRegionMgr();
        rm->initVarMgr();
        rm->init();
    }

    //Generate Program region.
    DexRegion * func_ru = (DexRegion*)rm->newRegion(REGION_FUNC);
    func_ru->initAttachInfoMgr();

    if (g_do_ipa) {
        ASSERT0(rumgr);
        //Allocate string buffer for region name used in ipa.
        CHAR * globalbuf = (CHAR*)((DexRegionMgr*)rumgr)->xmalloc(len);
        strcpy(globalbuf, runame);
        runame = globalbuf;
    }

    func_ru->setDexFile(df);
    func_ru->setDexMethod(dexm);
    func_ru->setRegionVar(rm->getVarMgr()->registerVar(
                        runame,
                        rm->getTypeMgr()->getMCType(0),
                        0, VAR_GLOBAL|VAR_FAKE|VAR_IS_FUNC));
    func_ru->setParamNum(lircode->numArgs);
    func_ru->setOrgVregNum(lircode->maxVars);
    func_ru->setClassSourceFilePath(getClassSourceFilePath(df, dexclassdef));
    DR_funcname(func_ru) = funcname;
    DR_classname(func_ru) = classname;
    DR_functype(func_ru) = functype;
    handleRegion(func_ru, df, lircode, fupool, dexm, dexcode, offsetvec);

    if (g_do_ipa) {
        Region * program = rumgr->getProgramRegion();
        ASSERT0(program);
        REGION_parent(func_ru) = program;
        program->addToVarTab(func_ru->getRegionVar());
        program->addToIRList(program->buildRegion(func_ru));
        rm->addToRegionTab(func_ru);
        if (rulist != nullptr) {
            //Caller must make sure func_ru will not be destroyed before IPA.
            rulist->append_tail(func_ru);
        }
    } else {
        ASSERT0(rumgr == nullptr);
        rm->deleteRegion(func_ru);
        delete rm;
    }

    //Convert to DEX code and store it to code buffer.
    //convertLIR2Dex(df, dexm, lircode, fupool);
    //dump_all_lir(lircode, df, dexm);
    return true;
}
