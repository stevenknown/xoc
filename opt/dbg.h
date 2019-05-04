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
#ifndef _DBG_H_
#define _DBG_H_

namespace xoc {

class Region;

//Describe debug information.
//Note line number can not be 0.
#define DBX_lineno(d)   ((d)->lineno)
class Dbx {
public:
    UINT lineno; //Note line number can not be 0.

public:
    Dbx() {}
    COPY_CONSTRUCTOR(Dbx);
    void clean() { lineno = 0; }
    void copy(Dbx const& dbx) { lineno = dbx.lineno; }
};


class DbxMgr {
public:
    class PrtCtx {
    public:
        //Record the string will be prefix of dbx information.
        char const* prefix;

    public:
        PrtCtx() { prefix = NULL; }
    };

public:
    DbxMgr() {}
    COPY_CONSTRUCTOR(DbxMgr);
    virtual ~DbxMgr() {}

    //Do some prepare work before print source file.
    virtual void doPrepareWorkBeforePrint() {}

    virtual void printSrcLine(IR const*, PrtCtx * ctx);
    virtual void printSrcLine(Dbx const*, PrtCtx *)
    {
        //Taget Dependent Code
        //Nothing to do.
    }
};


//User need to initialize DbxMgr before compilation.
extern DbxMgr * g_dbx_mgr;

//Copy Dbx from src.
void copyDbx(IR * tgt, IR const* src, Region * rg);
void setLineNum(IR * ir, UINT lineno, Region * rg);
UINT getLineNum(IR const* ir);
UINT getLineNum(Dbx const* dbx);
Dbx * getDbx(IR const* ir);

} //namespace xoc
#endif
