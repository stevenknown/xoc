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
#ifndef _CONFIG_H_
#define _CONFIG_H_

namespace xinfer {

typedef xcom::TMapIter<xoc::Sym const*, JNode const*> Sym2ConfIter;
typedef xcom::TMap<xoc::Sym const*, JNode const*> Sym2Conf;

class Config {
    COPY_CONSTRUCTOR(Config);
protected:
    int64_t m_eos_token_id;
    xoc::RegionMgr * m_rm;
    JNodeMgr & m_jm;
    xoc::ESymTab & m_sym_tab;
    Sym2Conf m_sym2conf;
protected:
    XI_STATUS extractConfig(JNode const* root);
public:
    Config(xoc::RegionMgr * rm, JNodeMgr & jm, xoc::ESymTab & sym_tab);
    ~Config();
    void dump() const;

    double extractNum(xoc::Sym const* name) const;
    bool extractBool(xoc::Sym const* name) const;
    xoc::Sym const* extractStr(xoc::Sym const* name) const;

    xoc::RegionMgr * getRegionMgr() const { return m_rm ; }
    JNodeMgr & getJNodeMgr() { return m_jm; }
    xoc::ESymTab & getSymTab() { return m_sym_tab; }
    int64_t get_eos_token_id() const { return m_eos_token_id; }

    XI_STATUS readConfig(CHAR const* filename);
};

} //namespace xinfer

#endif
