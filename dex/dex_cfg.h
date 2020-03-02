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
#ifndef _DEX_CFG_H_
#define _DEX_CFG_H_

class DEX_CFG : public IRCFG {
protected:
    void dump_node(FILE * h, bool detail, BitSet const& tryblock);
    void dump_node2(
            FILE * h,
            bool detail,
            BitSet const& trybbs,
            BitSet const& catchbbs);
    void dump_bb(
            FILE * h,
            bool detail,
            UINT id,
            UINT rpo,
            CHAR const* shape,
            CHAR const* color,
            CHAR const* font,
            UINT scale,
            OUT UINT & vertical_order);
public:
    DEX_CFG(CFG_SHAPE cs,
            BBList * bbl,
            Region * rg,
            UINT edge_hash_size = 16,
            UINT vertex_hash_size = 16) :
        IRCFG(cs, bbl, rg, edge_hash_size, vertex_hash_size) {}
    virtual ~DEX_CFG() {}

    void dump_vcg_ex(BitSet const& markbbs);
    void dump_vcg_ex2(BitSet const& trybbs, BitSet const& catchbbs);
};

#endif
