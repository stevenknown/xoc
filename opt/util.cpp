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
#include "../com/xcominc.h"
#include "errno.h"
#include "util.h"
using namespace xoc;

namespace xoc {

#define ERR_BUF_LEN 1024

//Print \l as the Carriage Return.
bool g_prt_carriage_return_for_dot = false;
INT g_indent = 0;
FILE * g_tfile = NULL;
static SMemPool * g_pool_tmp_used = NULL;
static CHAR g_indent_chars = ' ';

void interwarn(CHAR const* format, ...)
{
    StrBuf buf(64);
    va_list arg;
    va_start(arg, format);
    buf.vsprint(format, arg);
    prt2C("\n!!!INTERNAL WARNING:%s\n\n", buf.buf);
    va_end(arg);
}


//Print message to console.
void prt2C(CHAR const* format, ...)
{
    va_list args;
    va_start(args, format);
    #ifdef FOR_ANDROID
    __android_log_vprint(ANDROID_LOG_ERROR, LOG_TAG, format, args);
    #else
    vfprintf(stdout, format, args);
    #endif
    va_end(args);
    fflush(stdout);
}


void finidump()
{
    if (g_tfile != NULL) {
        fclose(g_tfile);
        g_tfile = NULL;
    }
}


SMemPool * get_tmp_pool()
{
    return g_pool_tmp_used;
}


void initdump(CHAR const* f, bool is_del)
{
    if (f == NULL) { return; }
    if (is_del) {
        UNLINK(f);
    }
    g_tfile = fopen(f, "a+");
    if (g_tfile == NULL) {
        fprintf(stderr, 
            "\ncan not open dump file %s, errno:%d, errstring:\'%s\'\n",
            f, errno, strerror(errno));
    }
}


//Print string with indent chars.
void note(CHAR const* format, ...)
{
    if (g_tfile == NULL || format == NULL) { return; }

    StrBuf buf(64);
    va_list arg;
    va_start(arg, format);
    buf.vstrcat(format, arg);
    
    size_t i = 0;
    while (i < buf.strlen()) {
        if (buf.buf[i] == '\n') {
            if (g_prt_carriage_return_for_dot) {
                //Print terminate lines that are left justified in DOT file.
                fprintf(g_tfile, "\\l");
            } else {
                fprintf(g_tfile, "\n");
            }
        } else {
            break;
        }
        i++;
    }

    //Append indent chars ahead of string.
    ASSERT0(g_indent >= 0);
    dumpIndent(g_tfile, g_indent);

    if (i == buf.strlen()) {
        fflush(g_tfile);
        goto FIN;
    }

    fprintf(g_tfile, "%s", buf.buf + i);
    fflush(g_tfile);
FIN:    
    va_end(arg);
    return;
}


//Malloc memory for tmp used.
void * tlloc(LONG size)
{
    if (size < 0 || size == 0) return NULL;
    if (g_pool_tmp_used == NULL) {
        g_pool_tmp_used = smpoolCreate(8, MEM_COMM);
    }
    void * p = smpoolMalloc(size, g_pool_tmp_used);
    if (p == NULL) return NULL;
    memset(p, 0, size);
    return p;
}


void tfree()
{
    if (g_pool_tmp_used != NULL) {
        smpoolDelete(g_pool_tmp_used);
        g_pool_tmp_used = NULL;
    }
}


void dumpIndent(FILE * h, UINT indent)
{
    for (; indent > 0; indent--) {
        fprintf(h, "%c", g_indent_chars);
    }
}


void dumpIntVector(Vector<UINT> & v)
{
    if (g_tfile == NULL) return;
    fprintf(g_tfile, "\n");
    for (INT i = 0; i <= v.get_last_idx(); i++) {
        UINT x = v.get(i);
        if (x == 0) {
            fprintf(g_tfile, "0,");
        } else {
            fprintf(g_tfile, "0x%x,", x);
        }
    }
    fflush(g_tfile);
}


//Integer TAB.
class IT : public RBT<int, int> {
public:
    //Add left child
    void add_lc(int from, RBCOL f, int to, RBCOL t)
    {
        RBTNType * x = m_root;
        if (x == NULL) {
            m_root = new_tn(from, f);
            x = new_tn(to, t);
            m_root->lchild = x;
            x->parent = m_root;
            return;
        }

        List<RBTNType*> lst;
        lst.append_tail(x);
        while (lst.get_elem_count() != 0) {
            x = lst.remove_head();
            if (x->key == from) {
                break;
            }
            if (x->rchild != NULL) {
                lst.append_tail(x->rchild);
            }
            if (x->lchild != NULL) {
                lst.append_tail(x->lchild);
            }
        }
        ASSERT0(x);
        ASSERT0(x->color == f);
        RBTNType * y = new_tn(to, t);

        ASSERT0(x->lchild == NULL);
        x->lchild = y;
        y->parent = x;
    }

    //Add left child
    void add_rc(int from, RBCOL f, int to, RBCOL t)
    {
        RBTNType * x = m_root;
        if (x == NULL) {
            m_root = new_tn(from, f);
            x = new_tn(to, t);
            m_root->rchild = x;
            x->parent = m_root;
            return;
        }

        List<RBTNType*> lst;
        lst.append_tail(x);
        while (lst.get_elem_count() != 0) {
            x = lst.remove_head();
            if (x->key == from) {
                break;
            }
            if (x->rchild != NULL) {
                lst.append_tail(x->rchild);
            }
            if (x->lchild != NULL) {
                lst.append_tail(x->lchild);
            }
        }
        ASSERT0(x);
        ASSERT0(x->color == f);
        RBTNType * y = new_tn(to, t);

        ASSERT0(x->rchild == NULL);
        x->rchild = y;
        y->parent = x;
    }
};


static void test1()
{
    IT x;
    x.add_lc(11, RBBLACK, 2, RBRED);
    x.add_rc(11, RBBLACK, 14, RBBLACK);

    x.add_lc(2, RBRED, 1, RBBLACK);
    x.add_rc(2, RBRED, 7, RBBLACK);

    x.add_lc(7, RBBLACK, 5, RBRED);
    x.add_rc(7, RBBLACK, 8, RBRED);

    x.add_rc(14, RBBLACK, 15, RBRED);

    //x.add_lc(5, RBRED, 4, RBRED);
    x.insert(4);
    dump_rbt(x);
}


static void test2()
{
    IT x;
    x.insert(1);
    x.insert(2);
    x.insert(11);
    x.insert(14);
    x.insert(15);
    x.insert(7);
    x.insert(5);
    x.insert(8);
    x.insert(4);
    x.insert(4);
    dump_rbt(x);

    //Test remove
    x.remove(7);
    x.remove(8);
    x.remove(4);
    x.remove(11);
    x.remove(14);
    x.remove(2);
    x.remove(5);
    x.remove(15);
    x.remove(1);

    //Test insert
    x.insert(1);
    x.insert(2);
    x.insert(11);
    x.insert(14);
    x.insert(15);
    x.insert(7);
    x.insert(5);
    x.insert(8);
    x.insert(4);
    x.insert(4);
    dump_rbt(x);
}


void test_rbt()
{
    test1();
    test2();
}

} //namespace xoc
