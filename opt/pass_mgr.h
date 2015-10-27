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
#ifndef __PASS_MGR_H__
#define __PASS_MGR_H__

namespace xoc {

//Time Info.
#define TI_pn(ti)        (ti)->pass_name
#define TI_pt(ti)        (ti)->pass_time
class TimeInfo {
public:
    CHAR const* pass_name;
    ULONGLONG pass_time;
};


class PassMgr {
protected:
    List<TimeInfo*> m_ti_list;
    SMemPool * m_pool;
    Region * m_ru;
    TypeMgr * m_dm;
    CDG * m_cdg;
    TMap<PASS_TYPE, Pass*> m_registered_pass;
    TMap<PASS_TYPE, Graph*> m_registered_graph_based_pass;

    void * xmalloc(size_t size)
    {
        void * p = smpoolMalloc(size, m_pool);
        if (p == NULL) return NULL;
        memset(p, 0, size);
        return p;
    }
    Graph * registerGraphBasedPass(PASS_TYPE opty);
public:
    PassMgr(Region * ru);
    COPY_CONSTRUCTOR(PassMgr);
    virtual ~PassMgr()
    {
        destroyPass();
        smpoolDelete(m_pool);
    }

    void appendTimeInfo(CHAR const* pass_name, ULONGLONG t)
    {
        TimeInfo * ti = (TimeInfo*)xmalloc(sizeof(TimeInfo));
        TI_pn(ti) = pass_name;
        TI_pt(ti) = t;
        m_ti_list.append_tail(ti);
    }

    virtual Graph * allocCDG();
    virtual Pass * allocCFG();
    virtual Pass * allocAA();
    virtual Pass * allocDUMgr();
    virtual Pass * allocCopyProp();
    virtual Pass * allocGCSE();
    virtual Pass * allocLCSE();
    virtual Pass * allocRP();
    virtual Pass * allocPRE();
    virtual Pass * allocIVR();
    virtual Pass * allocLICM();
    virtual Pass * allocDCE();
    virtual Pass * allocDSE();
    virtual Pass * allocRCE();
    virtual Pass * allocGVN();
    virtual Pass * allocLoopCvt();
    virtual Pass * allocSSAMgr();
    virtual Pass * allocCCP();
    virtual Pass * allocExprTab();
    virtual Pass * allocCfsMgr();

    void destroyPass();
    void dump_pass_time_info()
    {
        if (g_tfile == NULL) { return; }
        fprintf(g_tfile, "\n==---- PASS TIME INFO ----==");
        for (TimeInfo * ti = m_ti_list.get_head(); ti != NULL;
             ti = m_ti_list.get_next()) {
            fprintf(g_tfile, "\n * %s --- use %llu ms ---",
                    TI_pn(ti), TI_pt(ti));
        }
        fprintf(g_tfile, "\n===----------------------------------------===");
        fflush(g_tfile);
    }

    Pass * registerPass(PASS_TYPE opty);

    Pass * query_opt(PASS_TYPE opty)
    {
        if (opty == PASS_CDG) {
            return (Pass*)m_registered_graph_based_pass.get(opty);
        }
        return m_registered_pass.get(opty);
    }

    virtual void performScalarOpt(OptCTX & oc);
};

} //namespace xoc
#endif
