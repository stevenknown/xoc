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
#ifndef _MD_MGR_H_
#define _MD_MGR_H_

namespace xoc {

//The class represents API to manage the allocation of MD.
class MDMgr {
    COPY_CONSTRUCTOR(MDMgr);
protected:
    Region * m_rg;
    RegionMgr * m_rm;
    MDSystem * m_mdsys;
    TypeMgr * m_tm;
    VarMgr * m_vm;
protected:
    //This is a helper function to assignMD.
    void assignMDImpl(IR * x, bool assign_pr, bool assign_nonpr);
    MD const* allocSetElemMD(IR * ir);
public:
    MDMgr(Region * rg);
    ~MDMgr() {}

    //The function generates new MD for given PR.
    //It should be called if new PR generated in optimzations.
    MD const* allocIdMD(IR * ir);

    //Alloc MD for const string.
    MD const* allocStringMD(Sym const* string);

    //The function generates new MD for given direct memory reference.
    //It should be called if new IR generated in optimzations.
    //clean_mayset: true to clean MayRefSet of ir before function return.
    MD const* allocMDForDirectMemOp(IR * ir, bool clean_mayset);

    //The function generates new MD for given PR.
    //It should be called if new PR generated in optimzations.
    MD const* allocMDForPROp(IR * ir);

    //The function generates new MD for the IR tree that rooted by 'root'.
    //sibling: true if the function have to walk the sibling node of 'root'.
    //It should be called if new IR tree generated in optimzations.
    void allocRefForIRTree(IR * root, bool sibling);

    //The function generates new MD for the given IR.
    //NOTE the function will NOT process ir's kids and sibling.
    MD const* allocRef(IR * ir);

    //Assign MD for pr operations and nonpr direct memory operations.
    //is_only_assign_pr: true if assign MD for each ReadPR/WritePR operations.
    void assignMD(bool assign_pr, bool assign_nonpr);

    //Assign MD for pr operations and nonpr direct memory operations.
    //irlist: a list of IR to be assigned.
    //is_assign_pr: true if assign MD for each ReadPR/WritePR operations.
    //is_assign_nonpr: true if assign MD for each Non-PR memory operations.
    void assignMD(IR * irlist, bool assign_pr, bool assign_nonpr);

    //Assign MD for pr operations and nonpr direct memory operations.
    //The function will iterate given bblist.
    //is_only_assign_pr: true if only assign MD for read|write PR operations.
    void assignMDForBBList(BBList * lst, bool assign_pr, bool assign_nonpr);

    //Assign MD for pr operations and nonpr direct memory operations.
    //The function will iterate ir list in given bb.
    //is_only_assign_pr: true if only assign MD for read|write PR operations.
    void assignMDForBB(IRBB * bb, IRIter & ii,
                       bool assign_pr, bool assign_nonpr);

    //Assign MD for memory reference operations.
    //The function will iterate given ir list.
    //is_only_assign_pr: true if only assign MD for read|write PR operations.
    void assignMDForIRList(IR * lst, bool assign_pr, bool assign_nonpr);

    //Allocate MD for PR.
    MD const* genMDForPR(PRNO prno, Type const* type);

    //Generate MD corresponding to PR load or write.
    MD const* genMDForPR(IR const* ir)
    {
        ASSERT0(ir->isPROp());
        return genMDForPR(ir->getPrno(), ir->getType());
    }

    //Generate MD for Var.
    MD const* genMDForVar(Var * var, TMWORD offset)
    { return genMDForVar(var, var->getType(), offset); }

    //Generate MD for Var.
    MD const* genMDForVar(Var * var, Type const* type, TMWORD offset);

    //Generate MD for direct memory operation.
    MD const* genMDForDirectMemOp(IR const* ir)
    {
        ASSERT0(ir->isDirectMemOp());
        return genMDForVar(ir->getIdinfo(), ir->getType(), ir->getOffset());
    }
};

} //namespace xoc
#endif
