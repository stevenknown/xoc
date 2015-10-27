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

#define MAX_BUF_LEN 1024
#define ST_ERR  1
#define ST_SUCC 0
#define ST_EOF 2


//Conveninent to those classes which has a member named id.
template <class T> class ID_CK {
public:
    UINT compute_key(T t) const { return t->id; }
};


#define NIL_START  100000
template <class T, class Ttgt>
void dump_rbt(RBT<T, Ttgt> & rbt, CHAR const* name = NULL,
              UINT nil_count = NIL_START)
{
    typedef RBTNode<T, Ttgt> TN;
    Vector<TN*> nilvec;
    if (name == NULL) {
        name = "graph_rbt.vcg";
    }
    unlink(name);
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
            fprintf(hvcg,
                "\nnode: { title:\"%u\" label:\"%u\" shape:circle "
                "color:red fontname:\"courB\" textcolor:white}",
                key, key);
        } else {
            if (is_nil) {
                ASSERT0(key >= NIL_START);
                //nil
                fprintf(hvcg,
                    "\nnode: { title:\"%u\" label:\"%u\" shape:box "
                    "color:black fontname:\"courB\" textcolor:black}",
                    key, 0);
            } else {
                //black
                fprintf(hvcg,
                    "\nnode: { title:\"%u\" label:\"%u\" shape:circle "
                    "color:black fontname:\"courB\" textcolor:white}",
                    key, key);
            }
        }

        if (x->rchild != NULL) {
            lst.append_tail(x->rchild);
            fprintf(hvcg,
                    "\nedge: { sourcename:\"%u\" targetname:\"%u\" }",
                    key, x->rchild->key);
        } else if (!is_nil) {
            TN * nil = new TN();
            nil->key = nil_count++;
            nil->color = RBBLACK;
            nilvec.set(nilcc, nil);
            nilcc++;
            lst.append_tail(nil);

            fprintf(hvcg,
                    "\nedge: { sourcename:\"%u\" targetname:\"%u\" }",
                    key, nil->key);
        }

        if (x->lchild != NULL) {
            lst.append_tail(x->lchild);
            fprintf(hvcg,
                    "\nedge: { sourcename:\"%u\" targetname:\"%u\" }",
                    key, x->lchild->key);
        } else if (!is_nil) {
            TN * nil = new TN();
            nil->key = nil_count++;
            nil->color = RBBLACK;
            nilvec.set(nilcc, nil);
            nilcc++;
            lst.append_tail(nil);

            fprintf(hvcg,
                    "\nedge: { sourcename:\"%u\" targetname:\"%u\" }",
                    key, nil->key);
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
extern FILE * g_tfile;
extern INT g_indent; //Only for dump.
extern bool g_prt_carriage_return_for_dot; //Only for dump.

void dump_vec(Vector<UINT> & v);
void initdump(CHAR const* f, bool is_del);
void interwarn(CHAR const* format, ...);
void finidump();
void scr(CHAR const* format , ...) ;
void * tlloc(LONG size);
void tfree();
void note(CHAR const* format, ...);
INT prt(CHAR const* format , ...);
#endif
