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
#ifndef _MISC_ELF_MGR_H_
#define _MISC_ELF_MGR_H_

namespace elf {

//The class defines miscellaneous target machine ELFMgr.
class MiscELFMgr : public elf::ELFMgr {
protected:
    virtual void allocTargInfo();

    //Collector some factors about .sbss, .sdata, .bss, .data, .dl_spm
    //sections and section number of elf file for different architectures.
    virtual void collectELFFactor(OUT StringList & sym_name,
                                  OUT StringVec & func_name,
                                  OUT bool & has_sbss, OUT bool & has_sdata,
                                  OUT bool & has_bss, OUT bool & has_data,
                                  OUT bool & has_dlspm, OUT UINT & sbss_align,
                                  OUT UINT & sdata_align, OUT UINT & bss_align,
                                  OUT UINT & data_align,
                                  OUT UINT & dlspm_align,
                                  OUT UINT & shdr_num)
    { ASSERTN(0, ("TODO")); }

    //Compute a map saving function and its relocation data indexs.
    virtual void computeFuncRelInd(OUT xcom::Vector<UINT> & begin)
    { ASSERTN(0, ("TODO")); }

    //Generate contents for .data section for SW architectures.
    virtual void genBssContent(OUT elf::BYTEVec & bytevec)
    { ASSERTN(0, ("TODO")); }

    //Generate contents for .data section for SW architectures.
    virtual void genDataContent(OUT elf::BYTEVec & bytevec)
    { ASSERTN(0, ("TODO")); }

    //Generate contents for .dl_spm section. This function needs to be
    //implemented by subclasses according to different architectures.
    virtual void genDlSpmContent(OUT BYTEVec & bytevec)
    { ASSERTN(0, ("TODO")); }

    //Generate contents for .rel.text.xxx section for SW architectures.
    virtual void genFuncRelContent(OUT elf::BYTEVec & bytevec,
                                   elf::StringList const& names,
                                   xcom::Vector<UINT> const& begin,
                                   UINT const& i)
    { ASSERTN(0, ("TODO")); }

    //Generate contents for .text.xxx section for SW architectures.
    virtual void genFuncTextContent(OUT elf::BYTEVec & bytevec, UINT & ind)
    { ASSERTN(0, ("TODO")); }

    //Generate contents for .sbss section for SW architectures.
    virtual void genSbssContent(OUT elf::BYTEVec & bytevec)
    { ASSERTN(0, ("TODO")); }

    //Generate contents for .sdata section for SW architectures.
    virtual void genSdataContent(OUT elf::BYTEVec & bytevec)
    { ASSERTN(0, ("TODO")); }

    //Generate contents for .symtab section. This function needs to be
    //implemented by subclasses according to different architectures.
    virtual void genSymTabContent(OUT elf::BYTEVec & bytevec,
                                  elf::OffVec const& offvec,
                                  bool & has_sbss, bool & has_sdata,
                                  bool & has_bss, bool & has_data,
                                  bool & has_dlspm, UINT & sbss_align,
                                  UINT & sdata_align, UINT & bss_align,
                                  UINT & data_align, UINT & dlspm_align)
    { ASSERTN(0, ("TODO")); }
public:
    MiscELFMgr() {}
    virtual ~MiscELFMgr() {}
};

} //namespace

#endif
