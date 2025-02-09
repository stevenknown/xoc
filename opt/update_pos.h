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

#ifndef _UPDATE_POS_H_
#define _UPDATE_POS_H_

namespace xoc {

class LinearScanRA;

#define POS_UNDEF 0
#define POS_INIT_VAL 1

typedef UINT32 Pos;

class UpdatePos {
    bool m_use_expose;
    Pos m_pos;
    LinearScanRA const* m_ra;
protected:
    //Increase update position while building lifetime.
    void update(OUT Pos & defpos, OUT Pos & usepos)
    {
        usepos = m_pos++;
        defpos = m_pos++;
    }
public:
    UpdatePos(LinearScanRA const* ra) : m_ra(ra)
    { m_use_expose = true; m_pos = POS_INIT_VAL; }
    Pos getPos() const { return m_pos; }
    LinearScanRA const* getRA() const { return m_ra; }

    //Decrease position in step.
    static void dec(MOD Pos & pos) { pos--; }

    //Decrease position to lastest DEF.
    //e.g:given pos is 15 or 16, the output pos is 14.
    //  [14] <= [13]
    //  [16] <= [15]
    static void decToLastDef(MOD Pos & pos);

    //Decrease position to lastest DEF.
    //e.g:given pos is 16, the output pos is 15.
    //  [14] <= [13]
    //  [16] <= [15]
    static void decToLastUse(MOD Pos & pos);

    //Increase position to next DEF.
    //e.g:given pos is 16, the output pos is 16.
    //  [16] <= [15]
    //e.g2:given pos is 15, the output pos is 16.
    //  [16] <= [15]
    static void incToNextDef(MOD Pos & pos);

    //Increase position in step.
    static void inc(MOD Pos & pos) { pos++; }

    //Return true if pos indicates LHS.
    static bool isDef(Pos pos);

    //Return true if pos indicates RHS.
    static bool isUse(Pos pos);

    //Return true if user expect to generate Def/Use position at exposed-use
    //and exposed-def.
    bool useExpose() const { return m_use_expose; }
    bool updateAtRegionEntry(OUT Pos & dpos, OUT Pos & upos);
    bool updateAtRegionExit(OUT Pos & dpos, OUT Pos & upos);
    bool updateAtBBEntry(OUT Pos & dpos, OUT Pos & upos);
    bool updateAtBBExit(OUT Pos & dpos, OUT Pos & upos);
    virtual bool updateAtIR(IR const* ir, OUT Pos & dpos, OUT Pos & upos);

    void setUseExpose(bool use_exp) { m_use_expose = use_exp; }
};

} //namespace xoc
#endif
