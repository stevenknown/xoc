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
@*/
#ifndef _MIGEN_H_
#define _MIGEN_H_

namespace elf {
class ELFMgr;
}

namespace mach {

class MFieldMgr;
class MInst;
class MInstMgr;
class IR2MInst;
class MIRelocMgr;
class MIList;
class IMCtx;

class MIGen {
protected:
    SMemPool * m_pool;
    Region * m_rg;
    elf::ELFMgr * m_em;
    MFieldMgr * m_mfmgr;
    MInstMgr * m_mimgr;
    IR2MInst * m_ir2minst;
    MIRelocMgr * m_relocmgr;
protected:
    virtual IR2MInst * allocIR2MInst();
    virtual MFieldMgr * allocMFieldMgr();
    virtual MInstMgr * allocMInstMgr();
    virtual MIRelocMgr * allocMIRelocMgr();

    //Convert IRs to machine instructions.
    void convertIR2MI(OUT MIList & milst, MOD IMCtx * cont);

    //Convert generated machine instructions to binary codes.
    void convertMIListToCode(MIList const& milst);

    void destroy();
    void destroyMgr();

    //Generate debug_frame information.
    void genFrameInfo(MIList & mcfi_list);

    //Get memory alignment of target machine.
    virtual TMWORD getMemoryAlignment() const { return MEMORY_ALIGNMENT; }

    //Generate debug_line information.
    void genLineInfo(MIList & milst);
    Region * getRegion() const { return m_rg; }

    //Initialize class managers. Requires architecture specific allocation
    //functions.
    void initMgr();

    void performRelocation(MOD MIList & milst, MOD IMCtx * cont);

    //A helper function to extract values from "asdescvec" and set them into
    //fields of each mi.
    void setMIBinBuf(MOD mach::MInst * mi,
                     AssembleBinDescVec const& asdescvec);

    void * xmalloc(UINT size);
public:
    explicit MIGen(Region * rg, elf::ELFMgr * em);
    virtual ~MIGen() { destroy(); }

    void dump(MIList const& milst) const;

    //The function generates target dependent MI information.
    virtual bool perform();

    //Separate the instructions into regular instructions and CFI instructions.
    void separateMIListAndCFIList(MOD MInstMgr & imgr,
        MIList const& milst_all, MIList & milst, MIList & mcfi_list);
};

} //namespace mach

#endif
