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
#include "ops/ops.h"
#include "xinfer_cominc.h"
#include <cstdlib> // srand(), rand()
#include <ctime> // time()

namespace xinfer {

//
//START RTMemMgr
//
RTMemMgr::RTMemMgr()
{
    m_pool = xcom::smpoolCreate(64, MEM_COMM);
    m_rtt_count = 1;
}


RTMemMgr::~RTMemMgr()
{
    xcom::smpoolDelete(m_pool);
    m_pool = nullptr;
}


RTTensor * RTMemMgr::allocRTTensor()
{
    RTTensor * rtt = (RTTensor*)xmalloc(sizeof(RTTensor));
    #ifdef _DEBUG_
    rtt->id = m_rtt_count;
    m_rtt_count++;
    #endif
    return rtt;
}


RTTensor ** RTMemMgr::allocRTTensorArray(UINT elemnum)
{
    RTTensor ** rttarr = (RTTensor**)xmalloc(sizeof(RTTensor*) * elemnum);
    return rttarr;
}
//END RTMemMgr

} //namespace xinfer
