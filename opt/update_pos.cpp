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

#include "cominc.h"
#include "comopt.h"
#include "linear_scan.h"

namespace xoc {

//
//START UpdatePos
//
static inline bool is_even(Pos pos)
{
    return (pos & 0x1) == 0;
}


static inline bool is_odd(Pos pos)
{
    return (pos & 0x1) == 1;
}


void UpdatePos::decToLastUse(MOD Pos & pos)
{
    if (isDef(pos)) {
        dec(pos);
        return;
    }
}


void UpdatePos::incToNextDef(MOD Pos & pos)
{
    if (isUse(pos)) {
        inc(pos);
        return;
    }
}


void UpdatePos::decToLastDef(MOD Pos & pos)
{
    if (isDef(pos)) {
        dec(pos);
        dec(pos);
        return;
    }
    dec(pos);
}


//Return true if pos indicates LHS.
bool UpdatePos::isDef(Pos pos)
{
    return pos != POS_UNDEF && is_even(pos);
}


//Return true if pos indicates RHS.
bool UpdatePos::isUse(Pos pos)
{
    return pos != POS_UNDEF && is_odd(pos);
}


bool UpdatePos::updateAtRegionEntry(OUT Pos & dpos, OUT Pos & upos)
{
    update(dpos, upos);
    return true;
}


bool UpdatePos::updateAtRegionExit(OUT Pos & dpos, OUT Pos & upos)
{
    update(dpos, upos);
    return true;
}


bool UpdatePos::updateAtBBEntry(OUT Pos & dpos, OUT Pos & upos)
{
    if (useExpose()) {
        //BB start position
        update(dpos, upos);
        return true;
    }
    return false;
}


bool UpdatePos::updateAtBBExit(OUT Pos & dpos, OUT Pos & upos)
{
    if (useExpose()) {
        //BB end position
        update(dpos, upos);
        return true;
    }
    return false;
}


//Return true if ir will be encoded at current position, otherwise false that
//indicates ir is not belong to any lifetime.
bool UpdatePos::updateAtIR(IR const* ir, OUT Pos & dpos, OUT Pos & upos)
{
    ASSERT0(m_ra != nullptr);
    if (m_ra->isSpillOp(ir) || m_ra->isReloadOp(ir) ||
        m_ra->isRematOp(ir) || m_ra->isMoveOp(ir)) {
        //No need to handle spill/reload/remat/move.
        //Their occ did not encoded with a position and therefore not
        //resided in any lifetime.
        return false;
    }
    update(dpos, upos);
    return true;
}
//END UpdatePos

}