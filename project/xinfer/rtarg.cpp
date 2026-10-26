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
//START RTArg
//
RTArg::RTArg()
{
    clean();
    prompt_tokens.grow(128);
    gened_tokens.grow(128);
    history_tokens.grow(256);
}


void RTArg::dumpHistoryTokens(LLMInfer & infer, xoc::LogMgr * lm) const
{
    note(lm, "\nHISTORY TOKEN:total:%u", history_tokens.get_elem_count());
    if (history_tokens.get_elem_count() == 0) { return; }
    lm->incIndent(2);
    for (UINT i = 0; i < history_tokens.get_elem_count(); i++) {
        TOKID tid = history_tokens.get(i);
        dumpToken(infer, tid, i);
    }
    xcom::StrBuf utf8buf(128);
    dumpTokenVectorToUtf8Buffer(utf8buf, infer, history_tokens);
    note(lm, "\nHISTORY:%s\n", utf8buf.getBuf());
    lm->decIndent(2);
}


void RTArg::dumpPromptTokens(LLMInfer & infer, xoc::LogMgr * lm) const
{
    note(lm, "\nPROMPT TOKEN:total:%u", prompt_tokens.get_elem_count());
    if (prompt_tokens.get_elem_count() == 0) { return; }
    lm->incIndent(2);
    for (UINT i = 0; i < prompt_tokens.get_elem_count(); i++) {
        TOKID tid = prompt_tokens.get(i);
        dumpToken(infer, tid, i);
    }
    xcom::StrBuf utf8buf(128);
    dumpTokenVectorToUtf8Buffer(utf8buf, infer, prompt_tokens);
    note(lm, "\nPROMPT:%s\n", utf8buf.getBuf());
    lm->decIndent(2);
}


void RTArg::dumpGenedTokens(LLMInfer & infer, xoc::LogMgr * lm) const
{
    note(lm, "\nGENERATED TOKEN:total:%u", gened_tokens.get_elem_count());
    if (gened_tokens.get_elem_count() == 0) { return; }
    lm->incIndent(2);
    for (UINT i = 0; i < gened_tokens.get_elem_count(); i++) {
        TOKID tid = gened_tokens.get(i);
        dumpToken(infer, tid, i);
    }
    xcom::StrBuf utf8buf(128);
    dumpTokenVectorToUtf8Buffer(utf8buf, infer, gened_tokens);
    note(lm, "\nOUTPUT:%s\n", utf8buf.getBuf());
    lm->decIndent(2);
}


void RTArg::outputGenedTokens(LLMInfer & infer, xoc::LogMgr * lm) const
{
    if (gened_tokens.get_elem_count() == 0) { return; }
    xcom::StrBuf utf8buf(128);
    dumpTokenVectorToUtf8Buffer(utf8buf, infer, gened_tokens);
    fprintf(stdout, "\nOUTPUT:%s\n", utf8buf.getBuf());
}


void RTArg::reserveGenedToken()
{
    if (gened_tokens.get_elem_count() == 0) { return; }
    for (UINT i = 0; i < gened_tokens.get_elem_count(); i++) {
        TOKID tid = gened_tokens.get(i);
        history_tokens.append(tid);
    }
}


void RTArg::dump(LLMInfer & infer, xoc::LogMgr * lm) const
{
    if (lm == nullptr || !lm->is_init()) { return; }
    if (!g_dump_rtarg) { return; }
    note(lm, "\n-- RTArg --");
    lm->incIndent(2);
    note(lm, "\nn_embd:%u", n_embd);
    note(lm, "\nn_layer:%u", n_layer);
    note(lm, "\nn_head:%u", n_head);
    note(lm, "\nn_kvhead:%u", n_kvhead);
    note(lm, "\nn_positions:%u", n_positions);
    note(lm, "\nintermediate_size:%u", intermediate_size);
    note(lm, "\nrope_theta:%lf", rope_theta);
    note(lm, "\nlayernorm_epsilon:%lf", layernorm_epsilon);
    note(lm, "\nrmsnorm_epsilon:%lf", rmsnorm_epsilon);
    note(lm, "\nhidden bytesize:%u", hidden.get_capacity());
    note(lm, "\ntmp1 bytesize:%u", tmp1.get_capacity());
    note(lm, "\ntmp2 bytesize:%u", tmp2.get_capacity());
    note(lm, "\ntmp3 bytesize:%u", tmp3.get_capacity());
    note(lm, "\ntmp_qkv_and_fc bytesize:%u", tmp_qkv_and_fc.get_capacity());
    note(lm, "\nkcache bytesize:%u", kcache.get_capacity());
    note(lm, "\nvcache bytesize:%u", vcache.get_capacity());
    note(lm, "\ncurrent_pos:%u", current_pos);
    dumpPromptTokens(infer, lm);
    dumpGenedTokens(infer, lm);
    dumpHistoryTokens(infer, lm);
    lm->decIndent(2);
}


void RTArg::addGenedToken(TOKID tokid)
{
    ASSERT0(tokid < gened_tokens.get_vector_max_capacity());
    gened_tokens.append(tokid);
}


void RTArg::addPromptToken(TokenArray const* tokarr)
{
    ASSERT0(tokarr);
    ASSERT0(tokarr->array_elem_num <= prompt_tokens.get_vector_max_capacity());
    for (int64_t i = 0; i < tokarr->array_elem_num; i++) {
        prompt_tokens.append(tokarr->array[i]);
    }
}
//END RTArg

} //namespace xinfer
