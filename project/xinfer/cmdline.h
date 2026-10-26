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

#ifndef _CMDLINE_H_
#define _CMDLINE_H_

namespace xinfer {

extern CHAR const* g_dump_file_name;
extern CHAR const* g_model_name;
extern xcom::List<CHAR const*> g_safetensors_file_list;
extern xcom::List<CHAR const*> g_gguf_file_list;
extern xcom::List<CHAR const*> g_json_file_list;
extern xcom::List<CHAR const*> g_txt_file_list;
extern bool g_dump_tensor_data;
extern bool g_dump_json_parser;
extern CHAR const* g_prompt;
extern bool g_continuation;
extern bool g_chat;
extern bool g_dump_safetensors_mgr;
extern bool g_dump_config;
extern bool g_dump_tokenizer;
extern bool g_dump_rtarg;
extern bool g_dump_llminfer;
extern bool g_dump_rtctx;
extern bool g_dump_act;
extern bool g_dump_pieces;
extern bool g_dump_rt_norm;
extern bool g_dump_rt_vec;
extern bool g_dump_rt;
extern bool g_dump_rtctx;
extern float g_softmax_epsilon;

//-1 means dump all tensor data.
extern INT g_max_tensor_data_dump_num;
extern INT g_topk;
extern UINT g_max_seq_len;
extern HOST_FP g_temperature;

bool processCmdLine(INT argc, CHAR const* argv[]);

} //namespace xinfer
#endif
