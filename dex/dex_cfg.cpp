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
#include "libdex/DexFile.h"
#include "libdex/DexClass.h"
#include "libdex/DexProto.h"
#include "liropcode.h"
#include "lir.h"
#include "d2d_comm.h"
#include "drAlloc.h"
#include "cominc.h"
#include "comopt.h"
#include "dex.h"
#include "gra.h"
#include "dex_hook.h"

void DEX_CFG::dump_bb(
        FILE * h,
        bool detail,
        UINT id,
        UINT rpo,
        CHAR const* shape,
        CHAR const* color,
        CHAR const* font,
        UINT scale,
        OUT UINT & vertical_order)
{
    if (detail) {
        fprintf(h,
            "\nnode: {title:\"%d\" vertical_order:%d shape:%s color:%s "
            "fontname:\"%s\" scaling:%d label:\"",
            id, vertical_order++, shape, color, font, scale);
        fprintf(h, "   BB%d ", id);
        if (rpo != 0) {
            fprintf(h, " rpo:%d ", rpo);
        }

        IRBB * bb = getBB(id);
        ASSERT0(bb != NULL);
        dumpBBLabel(bb->getLabelList(), h);
        fprintf(h, "\n");
        for (IR * ir = BB_first_ir(bb); ir != NULL; ir = BB_next_ir(bb)) {
            //TODO: implement dump_ir_buf();
            //fprintf(h, "%s\n", dump_ir_buf(ir, buf));
            dumpIR(ir, m_tm, NULL, true, false);
        }
        fprintf(h, "\"}");
    } else {
        fprintf(h,
                "\nnode: {title:\"%d\" vertical_order:%d shape:%s color:%s "
                "fontname:\"%s\" scaling:%d label:\"%d",
                id, vertical_order++, shape, color, font, scale, id);
        if (rpo != 0) {
            fprintf(h, " rpo:%d", rpo);
        }
        fprintf(h, "\" }");
    }
}


void DEX_CFG::dump_node(FILE * h, bool detail, BitSet const& markbbs)
{
    C<IRBB*> * bbct;
    UINT vertical_order = 1;
    for (IRBB * bb = m_bb_list->get_head(&bbct);
         bb != NULL; bb = m_bb_list->get_next(&bbct)) {
        UINT id = BB_id(bb);
        UINT scale = 1;

        Vertex * v = getVertex(id);
        ASSERTN(v, ("bb is not in cfg"));

        CHAR const* shape = "box";
        if (BB_is_catch_start(bb)) {
            shape = "uptrapezoid";
        }

        CHAR const* font = "courB";
        CHAR const* color = "gold";
        if (BB_is_entry(bb) || BB_is_exit(bb)) {
            font = "Times Bold";
            scale = 2;
            color = "cyan";
        }

        if (markbbs.is_contain(BB_id(bb))) {
            color = "magenta";
        }

        dump_bb(h, detail, id,
                VERTEX_rpo(v), shape, color, font,
                scale, vertical_order);
    }
}


void DEX_CFG::dump_node2(
        FILE * h,
        bool detail,
        BitSet const& trybbs,
        BitSet const& catchbbs)
{
    C<IRBB*> * bbct;
    UINT vertical_order = 1;
    for (IRBB * bb = m_bb_list->get_head(&bbct);
         bb != NULL; bb = m_bb_list->get_next(&bbct)) {
        UINT id = BB_id(bb);
        UINT scale = 1;

        Vertex * v = getVertex(id);
        ASSERTN(v, ("bb is not in cfg"));

        CHAR const* shape = "box";
        if (BB_is_catch_start(bb)) {
            shape = "uptrapezoid";
        }

        CHAR const* font = "courB";
        CHAR const* color = "gold";
        if (BB_is_entry(bb) || BB_is_exit(bb)) {
            font = "Times Bold";
            scale = 2;
            color = "cyan";
        }

        if (trybbs.is_contain(BB_id(bb))) {
            color = "magenta";
        } else if (catchbbs.is_contain(BB_id(bb))) {
            color = "lightgreen";
        }

        dump_bb(h, detail, id, VERTEX_rpo(v),
                shape, color, font,
                scale, vertical_order);
    }
}


void DEX_CFG::dump_vcg_ex(BitSet const& markbbs)
{
    CHAR const* name = "graph_cfg.vcg";
    //Note this function does not use g_tfile as output.
    //So it is dispensable to check g_tfile.
    UNLINK(name);
    FILE * h = fopen(name, "a+");
    ASSERTN(h != NULL, ("%s create failed!!!",name));
    FILE * old = NULL;

    //Print comment
    //fprintf(h, "\n/*");
    //old = g_tfile;
    //g_tfile = h;
    //dumpBBList(m_bb_list, m_rg);
    //g_tfile = old;
    //fprintf(h, "\n*/\n");
    dump_head(h);

    //Print Region name.
    fprintf(h,
            "\nnode: {title:\"\" vertical_order:0 shape:box color:turquoise "
            "borderwidth:0 fontname:\"Courier Bold\" "
            "scaling:2 label:\"RegionName:%s\" }", m_rg->getRegionName());

    old = g_tfile;
    g_tfile = h;
    dump_node(h, true, markbbs);
    dump_edge(h, true);
    g_tfile = old;
    fprintf(h, "\n}\n");
    fclose(h);
}


void DEX_CFG::dump_vcg_ex2(BitSet const& trybbs, BitSet const& catchbbs)
{
    CHAR const* name = "graph_cfg.vcg";
    //Note this function does not use g_tfile as output.
    //So it is dispensable to check g_tfile.
    UNLINK(name);
    FILE * h = fopen(name, "a+");
    ASSERTN(h != NULL, ("%s create failed!!!",name));
    FILE * old = NULL;

    //Print comment
    //fprintf(h, "\n/*");
    //old = g_tfile;
    //g_tfile = h;
    //dumpBBList(m_bb_list, m_rg);
    //g_tfile = old;
    //fprintf(h, "\n*/\n");
    dump_head(h);

    //Print Region name.
    fprintf(h,
            "\nnode: {title:\"\" vertical_order:0 shape:box color:turquoise "
            "borderwidth:0 fontname:\"Courier Bold\" "
            "scaling:2 label:\"RegionName:%s\" }", m_rg->getRegionName());

    old = g_tfile;
    g_tfile = h;
    dump_node2(h, true, trybbs, catchbbs);
    dump_edge(h, true);
    g_tfile = old;
    fprintf(h, "\n}\n");
    fclose(h);
}
