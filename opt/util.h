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
#ifndef __UTIL_H__
#define __UTIL_H__

namespace xoc {

//Timer, show const string before timer start and end.
//e.g:
//    START_TIMER(t, "My Pass");
//    Run mypass();
//    END_TIMER(t, "My Pass");
#define START_TIMER(_timer_, s)                      \
    LONG _timer_ = 0;                                \
    if (g_show_comp_time) {                          \
        _timer_ = getclockstart();                   \
        prt2C("\n==-- START %s", (s));               \
    }
#define END_TIMER(_timer_, s)                        \
    if (g_show_comp_time) {                          \
        prt2C("\n==-- END %s", s);                   \
        prt2C(" Time:%fsec", getclockend(_timer_));  \
    }


//Timer, show format string before timer start and end.
//e.g:
//    START_TIMER(t, ("My Pass Name:%s", getPassName()));
//    Run mypass();
//    END_TIMER(t, ("My Pass Name:%s", getPassName()));
#define START_TIMER_FMT(_timer_, s)                  \
    LONG _timer_ = 0;                                \
    if (g_show_comp_time) {                          \
        _timer_ = getclockstart();                   \
        prt2C("\n==-- START ");                      \
        prt2C s;                                     \
    }
#define END_TIMER_FMT(_timer_, s)                    \
    if (g_show_comp_time) {                          \
        prt2C("\n==-- END ");                        \
        prt2C s;                                     \
        prt2C(" Time:%fsec", getclockend(_timer_)); \
    }


#define NIL_START  100000
#define DUMP_INDENT_NUM 4

//e.g:
//CHAR * dumpTN(SYM* key, SYM* mapped) { return SYM_name(key); }
//dump_rbt((RBT<SYM*, SYM*, xoc::CompareSymTab>&)map, NULL, 1000, dumpTN);
template <class T, class Ttgt, class CompareKey>
void dump_rbt(RBT<T, Ttgt, CompareKey> & rbt,
    CHAR const* name = NULL,
    UINT nil_count = NIL_START,
    CHAR const* (*dumpTN)(T, Ttgt) = NULL)
{
    typedef RBTNode<T, Ttgt> TN;
    Vector<TN*> nilvec;
    if (name == NULL) {
        name = "graph_rbt.vcg";
    }
    UNLINK(name);
    FILE * hvcg = fopen(name, "a+");
    ASSERT(hvcg, ("%s create failed!!!", name));
    fprintf(hvcg, "graph: {"
              "title: \"Tree\"\n"
              "shrink:  15\n"
              "stretch: 27\n"
              "layout_downfactor: 1\n"
              "layout_upfactor: 1\n"
              "layout_nearfactor: 1\n"
              "layout_splinefactor: 70\n"
              "spreadlevel: 1\n"
              "treefactor: 0.500000\n"
              "node_alignment: center\n"
              "orientation: top_to_bottom\n"
              "late_edge_labels: no\n"
              "display_edge_labels: yes\n"
              "dirty_edge_labels: no\n"
              "finetuning: no\n"
              "nearedges: no\n"
              "splines: yes\n"
              "ignoresingles: no\n"
              "straight_phase: no\n"
              "priority_phase: no\n"
              "manhatten_edges: no\n"
              "smanhatten_edges: no\n"
              "port_sharing: no\n"
              "crossingphase2: yes\n"
              "crossingoptimization: yes\n"
              "crossingweight: bary\n"
              "arrow_mode: free\n"
              "layoutalgorithm: tree\n"
              "node.borderwidth: 3\n"
              "node.color: lightcyan\n"
              "node.textcolor: darkred\n"
              "node.bordercolor: red\n"
              "edge.color: darkgreen\n");

    //Print node
    List<TN*> lst;
    TN const* root = rbt.get_root();
    if (root != NULL) {
        lst.append_tail(const_cast<TN*>(root));
    }

    UINT nilcc = 0;
    while (lst.get_elem_count() != 0) {
        TN * x = lst.remove_head();
        T key = T(0);
        bool is_nil = false;
        for (INT i = 0; i <= nilvec.get_last_idx(); i++) {
            TN * z = nilvec.get(i);
            if (z == NULL) { continue; }
            if (x == z) {
                key = z->key;
                is_nil = true;
                break;
            }
        }
        if (!is_nil) {
            key = x->key;
        }

        if (x->color == RBRED) {
            //red
            if (dumpTN != NULL) {
                fprintf(hvcg,
                    "\nnode: { title:\"%u\" label:\"%s\" shape:circle "
                    "color:red fontname:\"courB\" textcolor:white}",
                    (UINT)key, dumpTN(x->key, x->mapped));
            } else {
                fprintf(hvcg,
                    "\nnode: { title:\"%u\" label:\"%u\" shape:circle "
                    "color:red fontname:\"courB\" textcolor:white}",
                    (UINT)key, (UINT)key);
            }
        } else {
            if (is_nil) {
                ASSERT0(((UINT)key) >= NIL_START);
                //nil
                fprintf(hvcg,
                    "\nnode: { title:\"%u\" label:\"%u\" shape:box "
                    "color:black fontname:\"courB\" textcolor:black}",
                    (UINT)key, 0);
            } else {
                //black
                if (dumpTN != NULL) {
                    fprintf(hvcg,
                        "\nnode: { title:\"%u\" label:\"%s\" shape:circle "
                        "color:black fontname:\"courB\" textcolor:white}",
                        (UINT)key, dumpTN(x->key, x->mapped));
                } else {
                    fprintf(hvcg,
                        "\nnode: { title:\"%u\" label:\"%u\" shape:circle "
                        "color:black fontname:\"courB\" textcolor:white}",
                        (UINT)key, (UINT)key);
                }
            }
        }

        if (x->rchild != NULL) {
            lst.append_tail(x->rchild);
            fprintf(hvcg,
                "\nedge: { sourcename:\"%u\" targetname:\"%u\" }",
                (UINT)key, (UINT)x->rchild->key);
        } else if (!is_nil) {
            TN * nil = new TN();
            nil->key = (T)nil_count;
            nil_count++;
            nil->color = RBBLACK;
            nilvec.set(nilcc, nil);
            nilcc++;
            lst.append_tail(nil);

            fprintf(hvcg,
                "\nedge: { sourcename:\"%u\" targetname:\"%u\" }",
                (UINT)key, (UINT)nil->key);
        }

        if (x->lchild != NULL) {
            lst.append_tail(x->lchild);
            fprintf(hvcg,
                "\nedge: { sourcename:\"%u\" targetname:\"%u\" }",
                (UINT)key, (UINT)x->lchild->key);
        } else if (!is_nil) {
            TN * nil = new TN();
            nil->key = (T)nil_count;
            nil_count++;
            nil->color = RBBLACK;
            nilvec.set(nilcc, nil);
            nilcc++;
            lst.append_tail(nil);

            fprintf(hvcg,
                "\nedge: { sourcename:\"%u\" targetname:\"%u\" }",
                (UINT)key, (UINT)nil->key);
        }
    }
    for (INT i = 0; i <= nilvec.get_last_idx(); i++) {
        TN * z = nilvec.get(i);
        ASSERT0(z);
        delete z;
    }
    fprintf(hvcg, "\n}\n");
    fclose(hvcg);
}

//Exported Variables
extern FILE * g_tfile; //Only for dump.
extern INT g_indent; //Only for dump.
extern bool g_prt_carriage_return_for_dot; //Only for dump.

void dumpIndent(FILE * h, UINT indent);
void dumpIntVector(Vector<UINT> const& v);

//Get temporary memory pool handler.
SMemPool * get_tmp_pool();

//Initialze dump file.
void initdump(CHAR const* f, bool is_del);

//Finalize dump file.
void finidump();

//Report internal warning.
void interwarn(CHAR const* format, ...);

//Print message to console.
void prt2C(CHAR const* format, ...);

//Allocate memory from temporary memory pool.
void * tlloc(LONG size);

//Free whole temporary memory pool.
void tfree();

//Helper function to dump formatted string to g_tfile.
//This function indents blank space indicated by g_indent.
void note(CHAR const* format, ...);

//Helper function to dump formatted string to g_tfile without indent.
void prt(CHAR const* format, ...);

//Return true if val is 32bit integer more than 16bit.
bool isInteger32bit(HOST_UINT val);

//Return true if val is 64bit integer more than 32bit.
bool isInteger64bit(UINT64 val);

} //namespace xoc
#endif
