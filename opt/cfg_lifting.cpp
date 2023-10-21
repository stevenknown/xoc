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
#include "cominc.h"
#include "comopt.h"

namespace xoc {

CFGLifting::CFGLifting(IRCFG * cfg)
{
    ASSERT0(cfg);
    m_cfg = cfg;
    m_bb2li.createMap(cfg->getLoopInfo());
}


//Find and Return LOOP_SIBLING and BODY_ROOT.
//e.g:LOOP
//        BODY_ROOT
//    END_LOOP
//    LOOP_SIBLING
void CFGLifting::getKidOfLoop(IRBB * irbb, OUT IRBB ** sibling,
                              OUT IRBB ** body_root)
{
    LI<IRBB> * li = m_bb2li.get(irbb);
    ASSERT0(li != nullptr && li->getLoopHead() == irbb);
    List<IRBB*> succs;
    m_cfg->get_succs(succs, irbb);
    ASSERT0(succs.get_elem_count() == 2);
    IRBB * s = succs.get_head();
    if (sibling != nullptr) {
        *sibling = li->isInsideLoop(s->id()) ? succs.get_tail() : s;
    }

    if (body_root != nullptr) {
        *body_root = li->isInsideLoop(s->id()) ? s : succs.get_tail();
    }
}


//Find and Return TRUE_BODY, FALSE_BODY, IF_SIBLING.
//e.g:IF
//        TRUE_BODY
//    ELSE
//        FALSE_BODY
//    END_IF
//    IF_SIBLING
void CFGLifting::getKidOfIF(IRBB * irbb, IRBB ** true_body, IRBB ** false_body,
                            IRBB ** sibling)
{
    if (true_body != nullptr || false_body != nullptr) {
        IRBB * ipdom = m_cfg->get_ipdom(irbb);
        ASSERTN(ipdom, ("IRBB does not have ipdom"));
        IRBB * fallthrough_bb = m_cfg->getFallThroughBB(irbb);
        IRBB * target_bb = m_cfg->getTargetBB(irbb);
        IR * xr = m_cfg->get_last_xr(irbb);
        ASSERT0(xr != nullptr && xr->isConditionalBr());
        if (xr->is_truebr()) {
            if (true_body != nullptr) {
                if (ipdom == target_bb) {
                    *true_body = nullptr;
                } else {
                    *true_body = target_bb;
                }
            }
            if (false_body != nullptr) {
                if (ipdom == fallthrough_bb) {
                    *false_body = nullptr;
                } else {
                    *false_body = fallthrough_bb;
                }
            }
        } else {
            ASSERT0(xr->is_falsebr());
            if (true_body != nullptr) {
                if (ipdom == fallthrough_bb) {
                    *true_body = nullptr;
                } else {
                    *true_body = fallthrough_bb;
                }
            }
            if (false_body != nullptr) {
                if (ipdom == target_bb) {
                    *false_body = nullptr;
                } else {
                    *false_body = target_bb;
                }
            }
        } //end if
    }

    if (sibling != nullptr) {
        IRBB * ipdom = m_cfg->get_ipdom(irbb);
        ASSERTN(ipdom, ("irbb does not have ipdom"));
        *sibling = ipdom;
    }
}

} //namespace xoc
