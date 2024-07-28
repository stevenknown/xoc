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
#ifndef _MULTI_RES_CONVERT_H_
#define _MULTI_RES_CONVERT_H_

namespace xoc {

#define MAX_MULTI_RES_DESC_NUM 50

//This class converts multiple-result operation to normal IR list.
class MultiResConvert : public Pass {
    COPY_CONSTRUCTOR(MultiResConvert);
protected:
    ActMgr m_act_mgr;
    IRMgrExt * m_irmgr;
protected:
    IR * genPreDefStmt(
        IR * stmt, IR * res_isomo_list, IR ** predeflst);
    IR * genPostDefStmt(IR * stmt, IR * res_isomo_list);

    //The function returns the maximum number of multiple-result
    //descriptions allowed.
    UINT getMaxMultiResDescNum() const { return MAX_MULTI_RES_DESC_NUM; }
public:
    explicit MultiResConvert(Region * rg) : Pass(rg), m_act_mgr(rg)
    {
        ASSERT0(rg != nullptr);
        m_irmgr = (IRMgrExt*)rg->getIRMgr();
        ASSERT0(m_irmgr);
    }
    virtual ~MultiResConvert() {}

    //Build a store with a RHS expression which generate multiple results and
    //convert the store to a group stmts by inserting virtual-def.
    //e.g: given RHS expression is: broadcast src, $res1, $res2, res3, res4
    //which spreads 'ld src' to multiple result $res1, $res2, res3 and res4.
    //The generated single store is:
    //  st:u32 'res4'
    //    broadcast:u8
    //      ld:u32 'src'
    //      $res1:u8 multi-res
    //      $res2:u8 multi-res
    //      ld:u32 'res3' multi-res
    //      ld:u32 'res4' multi-res
    //After converting to multiple-result stmts by inserting vdef, the code
    //will be:
    //  vstpr $res1:u8 = broadcast:u8(ld 'src')  #S1
    //  vstpr $res2:u8 = broadcast:u8(ld 'src')  #S2
    //  vst:u32 'res3' = broadcast:u8(ld 'src')  #S3
    //  st:u32 'res4'                            #S4
    //      broadcast:u8
    //        ld:u32 'src'
    //        $res1:u8 multi-res
    //        $res2:u8 multi-res
    //        ld:u32 'res3' multi-res
    //Note original single store is almost unchanged, except the multi-res list
    //of broadcast expression. You may have noticed that the 4th kid of
    //broadcast, say 'ld res4', has been removed. The reason is the convertion
    //from normal-form-single-store to multiple-result-form-store will check
    //and make sure that each STMT has an individual result. Thus after the
    //convertion, broadcast's multi-res list expresses the remain results
    //beside the #S4.
    //Finally, the generated three stmts (#S1,#S2,#S3) and one existing
    //stmt (#S4) together express the multiple result of broadcast operation.
    IR * buildStorePRWithMultiResAndConvert(
        PRNO prno, Type const* type, IR * rhs);
    IR * buildStorePRWithMultiResAndConvert(Type const* type, IR * rhs);
    IR * buildStoreWithMultiResAndConvert(Var * lhs, IR * rhs);
    IR * buildStoreWithMultiResAndConvert(
        Var * lhs, Type const* type, IR * rhs);

    IR * convertMultiResMemMem(IR * stmt, IR * res1);
    IR * convertMultiResRegMem(IR * stmt, IR * res1);
    IR * convertMultiResRegReg(IR * stmt, IR * res1);

    //The function converts a stmt with multi-res-list descriptions, to a list
    //of normtal single-result stmts which are composed of Virtual OP.
    //e.g:Given stmt with multi-res-list descriptions is:
    //  st:u32 'res4'
    //    broadcast:u8
    //      ld:u32 'src'
    //      $res1:u8 multi-res
    //      $res2:u8 multi-res
    //      ld:u32 'res3' multi-res
    //      ld:u32 'res4' multi-res
    //Note the broadcast OP has one src operand and four multi-res descriptions.
    //After converting to the stmt, the returned stmt list is consist of three
    //Virtual OPs and original stmt:
    //  vstpr $res1:u8 = broadcast:u8(ld 'src')  #S1
    //  vstpr $res2:u8 = broadcast:u8(ld 'src')  #S2
    //  vst:u32 'res3' = broadcast:u8(ld 'src')  #S3
    //  st:u32 'res4'                            #S4
    //      broadcast:u8
    //        ld:u32 'src'
    //        $res1:u8 multi-res
    //        $res2:u8 multi-res
    //        ld:u32 'res3' multi-res
    //where #S1, #S2 and #S3 are Virtual OPs, and #S4 is original stmt.
    //genpostde: true to generate post virtual-def OP right after real-def stmt
    //  to prevent subsequent stmts from moving over.
    //  e.g: given res1, res2 = src,
    //  after convertion:
    //       vst res1 = src;
    //       st res2 = src, res1(dummyuse); #S5
    //       vst res1 = res1; #S6 (post virtual-def OP)
    //       ...
    //       st res1 = 10;    #S7
    //  #S6 prevents #S7 from scheduling over before #S6.
    IR * convertMultiRes(IR * stmt, bool genpostdef);

    virtual bool dump() const override;

    virtual CHAR const* getPassName() const override
    { return "Multiple Result Convert"; }
    PASS_TYPE getPassType() const { return PASS_MULTI_RES_CVT; }
    ActMgr const& getActMgr() const { return m_act_mgr; }
    IRMgrExt * getIRMgr() const { return m_irmgr; }
};

} //namespace xoc
#endif
