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
#ifndef _SAFETENSORS_MGR_H_
#define _SAFETENSORS_MGR_H_

namespace xinfer {

//The spec defined the maximum json header size is 100MB.
#define SFT_MAX_JSON_HEADER_LEN_SIZE (100*1024*1024)
#define SFT_JSON_HEAD_LEN_SIZE 8
#define SFT_MAX_TENSOR 16
#define SFT_NAME_BUF 128
#define SFT_DTYPE_BUF 32
#define SFT_MAX_DIM 8

class SafeTensor {
public:
    xoc::Sym const* name;
    xoc::DATA_TYPE dtype;
    UINT64 off_start;
    UINT64 off_end;
    xcom::Vector<UINT64> shape;
    xoc::BYTEVec data;
public:
    void dump(xoc::RegionMgr const* rm,
              bool dump_detail = xinfer::g_dump_tensor_data,
              INT dumpnum = 20) const;

    //Return the entire tensor bytesize.
    UINT64 getByteSize(xoc::RegionMgr const* rm) const;

    //Return the number of all elements in the tensor.
    UINT64 getTotalElemNum() const;

    //Return the bytesize of a single element.
    UINT getElemByteSize(xoc::RegionMgr const* rm) const;
    xcom::BYTE const* getDataBuf() const { return data.get_vec(); }
    UINT getDataBufLen() const { return data.get_capacity(); }
    xoc::DATA_TYPE getDType() const { return dtype; }
    UINT getShapeDimNum() const { return shape.get_elem_count(); }
    UINT getShapeDim(UINT idx) const
    {
        ASSERT0(idx < getShapeDimNum());
        return shape.get(idx);
    }

    //Load tensor data/weight from file.
    XI_STATUS loadData(
        FileObj & file, UINT64 fileoffset, xoc::RegionMgr const* rm);

    bool verify(xoc::RegionMgr const* rm) const;
};

//The class represents the structured information that extracted
//from safetensors files.
class STFileInfo {
public:
    UINT64 json_header_len;
    xoc::BYTEVec json_buf;
public:
    STFileInfo() { json_header_len = 0; }
    void dump(xoc::RegionMgr const* rm) const;
};

typedef xcom::TMapIter<xoc::Sym const*, SafeTensor*> Sym2SafeTensorIter;
typedef xcom::TMap<xoc::Sym const*, SafeTensor*> Sym2SafeTensor;

class SafeTensorsMgr {
    COPY_CONSTRUCTOR(SafeTensorsMgr);
protected:
    xoc::RegionMgr * m_rm;
    xoc::Sym const* m_metadata;
    xoc::Sym const* m_dtype;
    xoc::Sym const* m_shape;
    xoc::Sym const* m_data_offsets;
    xoc::ESymTab & m_sym_tab;
    JNodeMgr & m_jm;
    Sym2SafeTensor m_sym2st; 
    xcom::List<SafeTensor*> m_st_list;
protected:
    SafeTensor * allocSafeTensor();

    XI_STATUS extractTensorAttr(JNode const* jn, OUT SafeTensor & st);
    XI_STATUS extractSafeTensor(
        xcom::FileObj & file, UINT64 fileoffset, JNode const* root);
    XI_STATUS extractDataOffset(JNode const* jn, OUT SafeTensor & st);
    XI_STATUS extractShape(JNode const* jn, OUT SafeTensor & st);

    xoc::DATA_TYPE mapStrToDType(xoc::Sym const* str) const;
public:
    SafeTensorsMgr(RegionMgr * rm, JNodeMgr & jm, xoc::ESymTab & sym_tab);
    ~SafeTensorsMgr();

    xoc::Sym const* addToSymbolTab(CHAR const* s);

    void dump() const;

    xoc::RegionMgr * getRegionMgr() const { return m_rm ; }
    xoc::ESymTab & getSymTab() { return m_sym_tab; }
    JNodeMgr & getJNodeMgr() { return m_jm; }
    SafeTensor const* getSafeTensor(CHAR const* name);
    SafeTensor const* getSafeTensor(xoc::Sym const* name) const
    { return m_sym2st.get(name); }

    XI_STATUS readSafeTensors(CHAR const* filename);

    bool verify(xoc::RegionMgr const* rm) const;
};

} //namespace xinfer

#endif
