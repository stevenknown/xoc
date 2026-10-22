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
#include "xinfer_cominc.h"

namespace xinfer {

//
//START SafeTensor
//
void SafeTensor::dump(
    xoc::RegionMgr const* rm, bool dump_detail, INT dumpnum) const
{
    xoc::LogMgr * lm = rm->getLogMgr();
    if (lm == nullptr || !lm->is_init()) { return; }
    note(lm, "\nname:");
    if (name != nullptr) {
        prt(lm, "%s", name->getStr());
    }
    note(lm, "\ndtype:%s,bytesize:%u",
        xoc::TypeMgr::getDTypeName(dtype), getElemByteSize(rm));
    note(lm, "\noff_start:%u", off_start);
    note(lm, "\noff_end:%u", off_end);
    note(lm, "\nshape:");
    if (shape.get_elem_count() > 0) {
        prt(lm, "[");
        for (UINT i = 0; i < shape.get_elem_count(); i++) {
            UINT64 dim = shape.get(i);
            ASSERT0(dim != 0);
            if (i != 0) { prt(lm, ","); }
            prt(lm, "%u", dim);
        }
        prt(lm, "]");
        prt(lm, ",total:%u", getTotalElemNum());
    }
    note(lm, "\ndata:%u bytes", getByteSize(rm));
    if (data.get_elem_count() > 0 && dump_detail) {
        lm->incIndent(2);
        data.dump(lm, dumpnum);
        lm->decIndent(2);
    }
}


bool SafeTensor::verify(xoc::RegionMgr const* rm) const
{
    ASSERT0(off_start <= off_end);
    UINT filebytesize = off_end - off_start;
    UINT tensorbytesize = getByteSize(rm);
    ASSERT0(filebytesize == tensorbytesize);
    return true;
}


UINT SafeTensor::getElemByteSize(xoc::RegionMgr const* rm) const
{
    ASSERT0(dtype != D_UNDEF);
    return rm->getTypeMgr()->getDTypeByteSize(dtype);
}


UINT64 SafeTensor::getByteSize(xoc::RegionMgr const* rm) const
{
    UINT64 total = getTotalElemNum();
    UINT elem_byte_size = getElemByteSize(rm);
    ASSERT0(total != 0);
    ASSERT0(elem_byte_size != 0);
    return total * elem_byte_size;
}


UINT64 SafeTensor::getTotalElemNum() const
{
    UINT64 total = 1;
    for (UINT i = 0; i < shape.get_elem_count(); i++) {
        UINT64 dim = shape.get(i);
        ASSERT0(dim != 0);
        total = total * dim;
    }
    return total;
}


XI_STATUS SafeTensor::loadData(
    xcom::FileObj & file, UINT64 fileoffset, xoc::RegionMgr const* rm)
{
    size_t rd = 0;
    UINT64 tensorbytesize = getByteSize(rm);
    if (data.get_capacity() < tensorbytesize) {
        data.set(tensorbytesize, 0);
    }
    ASSERT0((off_end - off_start) == tensorbytesize);
    xcom::FO_STATUS status = file.read(
        data.get_vec(), fileoffset + off_start, tensorbytesize, &rd);
    if (status != xcom::FO_SUCC) {
        dumpAct(rm, "\nerror:read tensor data %u bytes failed.",
                tensorbytesize);
        return XI_ERR;
    }
    ASSERT0(rd == tensorbytesize);
    return XI_SUCC;
}
//END SafeTensor


//
//START STFileInfo
//
void STFileInfo::dump(xoc::RegionMgr const* rm) const
{
    xoc::LogMgr * lm = rm->getLogMgr();
    if (lm == nullptr || !lm->is_init()) { return; }
    note(lm, "\n==-- DUMP STFileInfo --==");
    lm->incIndent(2);
    note(lm, "\njson_header_len:%u", json_header_len);
    note(lm, "\njson_header_content_bytesize:%u", json_buf.get_elem_count());
    lm->incIndent(2);
    json_buf.dump(lm);
    lm->decIndent(2);
    lm->decIndent(2);
}
//END STFileInfo


//
//START SafeTensorsMgr
//
SafeTensorsMgr::SafeTensorsMgr(
    RegionMgr * rm, JNodeMgr & jm, xoc::ESymTab & sym_tab)
    : m_sym_tab(sym_tab), m_jm(jm)
{
    ASSERT0(rm);
    m_rm = rm;
    m_metadata = addToSymbolTab("__metadata__");
    m_dtype = addToSymbolTab("dtype");
    m_shape = addToSymbolTab("shape");
    m_data_offsets = addToSymbolTab("data_offsets");
}


SafeTensorsMgr::~SafeTensorsMgr()
{
    for (SafeTensor * st = m_st_list.get_head();
         st != nullptr; st = m_st_list.get_next()) {
        delete st;
    }
    m_st_list.clean();
}


static bool isValidJsonHeadLen(UINT64 json_header_len)
{
    return json_header_len <= SFT_MAX_JSON_HEADER_LEN_SIZE;
}


SafeTensor const* SafeTensorsMgr::getSafeTensor(CHAR const* name)
{
    xoc::Sym const* namesym = addToSymbolTab(name);
    return getSafeTensor(namesym);
}


void SafeTensorsMgr::dump() const
{
    if (!m_rm->isLogMgrInit()) { return; }
    if (!g_dump_safetensors_mgr) { return; }
    note(m_rm, "\n==-- DUMP SafeTensorsMgr --==");
    xoc::LogMgr * lm = m_rm->getLogMgr();
    lm->incIndent(2);

    note(lm, "\ntensors:%u", m_sym2st.get_elem_count());
    lm->incIndent(2);
    SafeTensor * st;
    Sym2SafeTensorIter it;
    for (xoc::Sym const* sym = m_sym2st.get_first(it, &st);
         sym != nullptr; sym = m_sym2st.get_next(it, &st)) {
        note(lm, "\n----");
        st->dump(m_rm);
    }
    lm->decIndent(2);

    lm->decIndent(2);
}


bool SafeTensorsMgr::verify(xoc::RegionMgr const* rm) const
{
    SafeTensor * st;
    Sym2SafeTensorIter it;
    for (xoc::Sym const* sym = m_sym2st.get_first(it, &st);
         sym != nullptr; sym = m_sym2st.get_next(it, &st)) {
        ASSERT0(st->verify(rm));
    }
    return true;
}


xoc::Sym const* SafeTensorsMgr::addToSymbolTab(CHAR const* s)
{
    return m_sym_tab.add(s);
}


SafeTensor * SafeTensorsMgr::allocSafeTensor()
{
    SafeTensor * st = new SafeTensor();
    m_st_list.append_tail(st);
    return st;
}


XI_STATUS SafeTensorsMgr::extractTensorAttr(
    JNode const* jn, OUT SafeTensor & st)
{
    if (!jn->is_obj()) { return XI_INVALID_TENSOR_ATTR; }
    for (JNode const* x = JOBJ_pair_list(jn);
         x!= nullptr; x = x->next) {
        ASSERT0(x->is_pair());
        if (JPAIR_key(x) == m_dtype) {
            xoc::DATA_TYPE dt;
            XI_STATUS status = m_jm.extractDType(JPAIR_val(x), dt);
            st.dtype = dt;
            if (status != XI_SUCC) { return status; }
            continue;
        }
        if (JPAIR_key(x) == m_shape) {
            XI_STATUS status = extractShape(JPAIR_val(x), st);
            if (status != XI_SUCC) { return status; }
            continue;
        }
        if (JPAIR_key(x) == m_data_offsets) {
            XI_STATUS status = extractDataOffset(JPAIR_val(x), st);
            if (status != XI_SUCC) { return status; }
            continue;
        }
        ASSERTN(0, ("TODO"));
    }
    return XI_SUCC;
}


XI_STATUS SafeTensorsMgr::extractDataOffset(
    JNode const* jn, OUT SafeTensor & st)
{
    ASSERT0(jn);
    ASSERT0(jn->is_array());
    ASSERT0(xcom::cnt_list(JARRAY_elem_list(jn)) == 2);
    JNode const* jn_start = JARRAY_elem_list(jn);
    JNode const* jn_end = JARRAY_elem_list(jn)->next;

    double num;
    XI_STATUS status;
    status = m_jm.extractNumber(jn_start, num);
    if (status != XI_SUCC) { return status; }
    st.off_start = (UINT64)num;

    status = m_jm.extractNumber(jn_end, num);
    if (status != XI_SUCC) { return status; }
    st.off_end = (UINT64)num;
    return XI_SUCC;
}


XI_STATUS SafeTensorsMgr::extractShape(
    JNode const* jn, OUT SafeTensor & st)
{
    ASSERT0(jn);
    ASSERT0(jn->is_array());
    UINT i = 0;
    XI_STATUS status;
    for (JNode const* e = JARRAY_elem_list(jn);
         e != nullptr; e = e->next, i++) {
        double num;
        status = m_jm.extractNumber(e, num);
        if (status != XI_SUCC) { return status; }
        st.shape.set(i, (UINT64)num);
    }
    return XI_SUCC;
}


XI_STATUS SafeTensorsMgr::extractSafeTensor(
    xcom::FileObj & file, UINT64 fileoffset, JNode const* root)
{
    ASSERT0(root);
    ASSERT0(root->is_obj());
    for (JNode const* jn = JOBJ_pair_list(root);
         jn != nullptr; jn = jn->next) {
        ASSERT0(jn->is_pair());
        if (JPAIR_key(jn) == m_metadata) { continue; }
        SafeTensor * st = allocSafeTensor();
        st->name = JPAIR_key(jn);
        m_sym2st.set(st->name, st);

        //Load tensor's attributes.
        XI_STATUS status = extractTensorAttr(JPAIR_val(jn), *st);
        if (status != XI_SUCC) { return status; }

        //Load tensor's data.
        status = st->loadData(file, fileoffset, m_rm);
        if (status != XI_SUCC) { return status; }

    }
    return XI_SUCC;
}


XI_STATUS SafeTensorsMgr::readSafeTensors(CHAR const* filename)
{
    xcom::FO_STATUS st;
    bool is_del = false;
    bool is_readonly = false;
    xcom::FileObj sftfile(filename, is_del, is_readonly, &st);
    if (st != xcom::FO_SUCC) {
        xoc::prt2C("error:read \"%s\" failed.", filename);
        return XI_ERR;
    }
    STFileInfo si;
    UINT64 fileoffset = 0;

    //1. json header length
    size_t rd = 0;
    st = sftfile.read(
        (BYTE*)&si.json_header_len, fileoffset, SFT_JSON_HEAD_LEN_SIZE, &rd);
    if (st != xcom::FO_SUCC) {
        xoc::prt2C("error:read header failed.\n", filename);
        return XI_ERR;
    }
    ASSERT0(rd == SFT_JSON_HEAD_LEN_SIZE);
    if (!isValidJsonHeadLen(si.json_header_len)) { return XI_OOM; }
    si.json_buf.grow(si.json_header_len);
    fileoffset += SFT_JSON_HEAD_LEN_SIZE;

    //2. json header content
    rd = 0;
    st = sftfile.read(
        si.json_buf.get_vec(), fileoffset, si.json_header_len, &rd);
    if (st != xcom::FO_SUCC) {
        xoc::prt2C("error:read header failed.\n", filename);
        return XI_ERR;
    }
    fileoffset += si.json_header_len;
    ASSERT0(si.json_header_len == rd);
    si.json_buf.set(si.json_header_len, 0);

    JsonParser jp(m_rm->getLogMgr(), getJNodeMgr());
    JNode const* root = jp.parse(
        (CHAR const*)si.json_buf.get_vec(), si.json_buf.get_capacity());
    if (root == nullptr) { return XI_ERR; }

    //3. tensor data content.
    extractSafeTensor(sftfile, fileoffset, root);
    dump();
    ASSERT0(verify(m_rm));
    return XI_SUCC;
}
//END SafeTensorsMgr

} //namespace xinfer
