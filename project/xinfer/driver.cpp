/*@
Copyright (c) 2026-present, Su Zhenyu steven.known@gmail.com

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

//
//START Driver
//
xoc::RegionMgr * Driver::initRegionMgr()
{
    if (m_rm != nullptr) { return m_rm; }
    RegionMgr * rm = new ARMRegionMgr();
    rm->initTypeMgr();
    rm->initVarMgr();
    rm->initIRDescFlagSet();
    rm->initTargInfo();
    #ifdef REF_TARGMACH_INFO
    rm->initTargInfoMgr();
    #endif
    m_rm = rm;
    return m_rm;
}


Driver::~Driver()
{
    if (m_rm != nullptr) { delete m_rm; m_rm = nullptr; }
}


bool Driver::handleSafeTensorsFileList(OUT SafeTensorsMgr & stmgr)
{
    for (CHAR const* fn = xinfer::g_safetensors_file_list.get_head();
         fn != nullptr; fn = xinfer::g_safetensors_file_list.get_next()) {
        XI_STATUS stm = stmgr.readSafeTensors(fn);
        if (stm != XI_SUCC) { return false; }
    }
    return true;
}


bool Driver::handleModelName(xoc::RegionMgr * rm, OUT ModelType & mt)
{
    if (g_model_name == nullptr) {
        fprintf(stderr, "error:miss model name");
        return false;
    }
    if (::strcmp(g_model_name, "distilgpt2") == 0) {
        mt = MODEL_DISTILGPT2;
        return true;
    }
    if (::strcmp(g_model_name, "gpt2") == 0) {
        mt = MODEL_GPT2;
        return true;
    }
    if (::strcmp(g_model_name, "qwen2.5_1.5B") == 0) {
        mt = MODEL_QWEN2D5_1D5B;
        return true;
    }
    dumpAct(rm->getLogMgr(), "\nunknown model name:%s", g_model_name);
    return false;
}


bool Driver::handleTXTFileList(OUT Tokenizer & tokenizer)
{
    xcom::StrBuf sbuf(32);
    for (CHAR const* fn = xinfer::g_txt_file_list.get_head();
         fn != nullptr; fn = xinfer::g_json_file_list.get_next()) {
        sbuf.growBuf(::strlen(fn));
        CHAR const* filename =
            xcom::getFileName(fn, sbuf.getBuf(), sbuf.getBufLen(), false);
        if (::strcmp(filename, "merges.txt") == 0) {
            tokenizer.readMergesTxt(fn);
        }
    }
    return true;
}


bool Driver::handleJSONFileList(OUT Config & conf, OUT Tokenizer & tokenizer)
{
    xcom::StrBuf sbuf(32);
    for (CHAR const* fn = xinfer::g_json_file_list.get_head();
         fn != nullptr; fn = xinfer::g_json_file_list.get_next()) {
        START_TIMER_FMT(t, ("handleJSONFile:%s", fn));
        sbuf.growBuf(::strlen(fn)+100);
        CHAR const* filename =
            xcom::getFileName(fn, sbuf.getBuf(), sbuf.getBufLen(), false);
        if (::strcmp(filename, "config.json") == 0) {
            conf.readConfig(fn);
        }
        if (::strcmp(filename, "vocab.json") == 0) {
            tokenizer.readVocab(fn);
        }
        if (::strcmp(filename, "tokenizer.json") == 0) {
            tokenizer.readTokenizer(fn);
        }
        if (::strcmp(filename, "tokenizer_config.json") == 0) {
            tokenizer.readConfig(fn);
        }
        END_TIMER_FMT(t, ("handleJSONFile:%s", fn));
    }
    return true;
}


bool Driver::perform()
{
    xoc::RegionMgr * rm = initRegionMgr();
    if (g_dump_file_name != nullptr) {
        rm->getLogMgr()->init(g_dump_file_name, true);
    }
    ModelType mt;
    if (!handleModelName(rm, mt)) { return false; }
    ASSERT0(mt != MODEL_UNDEF);
    xoc::ESymTab sym_tab;
    JNodeMgr jm(sym_tab);
    xoc::Option::dump(rm);
    SafeTensorsMgr stmgr(rm, jm, sym_tab);
    Config conf(rm, jm, sym_tab);
    Tokenizer tokenizer(rm, jm, sym_tab);
    if (!handleSafeTensorsFileList(stmgr)) { return false; }
    if (!handleJSONFileList(conf, tokenizer)) { return false; }
    if (!handleTXTFileList(tokenizer)) { return false; }
    bool run_test = true;
    if (run_test) {
        TestOps testops(m_rm, stmgr);
        testops.perform();
    }
    conf.dump();
    tokenizer.dump();
    ModelMgr mm;
    LLMInfer * infer = mm.allocModel(
        mt, m_rm, stmgr, conf, jm, tokenizer, sym_tab);
    ASSERT0(infer);
    bool succ = infer->perform();
    return succ;
}
//END Driver

} //namespace xinfer
