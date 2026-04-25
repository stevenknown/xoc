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

author: Su Zhenyu
@*/
#ifndef _MI_DESC_H_
#define _MI_DESC_H_

namespace mach {

#define IRSI_last_result_avail_cyc(si) ((si)->last_result_avail_cyc)
#define IRSI_first_result_avail_cyc(si) ((si)->first_result_avail_cyc)
#define IRSI_reg_result_avail_cyc(si, i) ((si)->reg_result_cyc_buf[(i)])
#define IRSI_mem_result_avail_cyc(si, i) ((si)->mem_result_cyc_buf[(i)])
#define IRSI_reg_result_cyc_buf(si) ((si)->reg_result_cyc_buf)
#define IRSI_mem_result_cyc_buf(si) ((si)->mem_result_cyc_buf)
#define IRSI_occ_excl_latency(si) ((si)->occupy_exclusive_latency)
typedef struct tagIRScheInfo {
public:
    UINT * reg_result_cyc_buf; //record each of register result available cycle
    UINT * mem_result_cyc_buf; //record each of memory result available cycle
    UINT last_result_avail_cyc; //record last result available cycle
    UINT first_result_avail_cyc; //record first result available cycle

    //Record occupy-exclusive latency.
    //e.g: For some target machine, float-division function unit occupies the
    //execution pipeline exclusively for more than 10 cycles.
    UINT occupy_exclusive_latency;
public:
    void init()
    {
        reg_result_cyc_buf = nullptr;
        mem_result_cyc_buf = nullptr;
        last_result_avail_cyc = 0;
        first_result_avail_cyc = 0;
        occupy_exclusive_latency = 0;
    }
} IRScheInfo;


#define MTD_code(mi) ((mi)->m_code)
#define MTD_name(mi) ((mi)->m_name)
#define MTD_unit(mi) ((mi)->m_unit)
#define MTD_sche_info(mi) ((mi)->m_sche_info)
#define MTD_is_store(mi) ((mi)->m_is_store)
class MICodeDesc {
public:
    MI_CODE m_code;
    const char* m_name;
    UNIT m_unit;
    IRScheInfo m_sche_info;
    UINT m_is_store:1;
};

} //namespace mach
#endif
