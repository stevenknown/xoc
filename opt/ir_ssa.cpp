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
#include "cominc.h"
#include "comopt.h"

namespace xoc {

//
//START DfMgr
//
//Get the BB set where 'v' is the dominate frontier of them.
xcom::BitSet * DfMgr::genDFControlSet(UINT vid)
{
    xcom::BitSet * df = m_df_vec.get(vid);
    if (df == NULL) {
        df = m_bs_mgr.create();
        m_df_vec.set(vid, df);
    }
    return df;
}


void DfMgr::clean()
{
    for (INT i = 0; i <= m_df_vec.get_last_idx(); i++) {
        xcom::BitSet * df = m_df_vec.get(i);
        if (df != NULL) {
            df->clean();
        }
    }
}


//g: dump dominance frontier to graph.
void DfMgr::dump(xcom::DGraph & g)
{
    if (g_tfile == NULL) { return; }
    note("\n==---- DUMP Dominator Frontier Control Set ----==\n");
    INT c;
    for (xcom::Vertex const* v = g.get_first_vertex(c);
         v != NULL; v = g.get_next_vertex(c)) {
        UINT vid = VERTEX_id(v);
        note("\nBB%d DF set:", vid);
        xcom::BitSet const* df = m_df_vec.get(vid);
        if (df != NULL) {
            for (INT i = df->get_first(); i >= 0; i = df->get_next(i)) {
                prt("%d,", i);
            }
        }
    }
    fflush(g_tfile);
}


//This function compute dominance frontier to graph g.
void DfMgr::buildRecur(xcom::Vertex const* v, xcom::DGraph const& g, DomTree const& domtree)
{
    UINT vid = VERTEX_id(v);
    xcom::Vertex * v_domtree = domtree.get_vertex(vid);
    ASSERT0(v_domtree);

    //Access each succs.
    for (xcom::EdgeC const* ec = VERTEX_out_list(v_domtree);
         ec != NULL; ec = EC_next(ec)) {
        xcom::Vertex const* succ_domtree = EDGE_to(EC_edge(ec));
        xcom::Vertex const* succ = g.get_vertex(VERTEX_id(succ_domtree));
        buildRecur(succ, g, domtree);
    }

    xcom::BitSet * df = genDFControlSet(vid);
    df->clean();

    //Compute DF(local)
    for (xcom::EdgeC const* ec = VERTEX_out_list(v);
         ec != NULL; ec = EC_next(ec)) {
        xcom::Vertex const* succ = EDGE_to(EC_edge(ec));
        if (g.get_idom(VERTEX_id(succ)) != vid) {
            df->bunion(VERTEX_id(succ));
        }
    }

    //Compute DF(up)
    for (xcom::EdgeC const* ec = VERTEX_out_list(v_domtree);
         ec != NULL; ec = EC_next(ec)) {
        xcom::Vertex const* succ_domtree = EDGE_to(EC_edge(ec));
        xcom::BitSet * succ_df = genDFControlSet(VERTEX_id(succ_domtree));
        for (INT p = succ_df->get_first(); p >= 0; p = succ_df->get_next(p)) {
            if (g.get_idom((UINT)p) != vid) {
                df->bunion(p);
            }
        }
    }
}


//This function compute dominance frontier to graph g recursively.
void DfMgr::build(xcom::DGraph const& g, DomTree const& domtree)
{
    INT c;
    for (xcom::Vertex const* v = g.get_first_vertex(c);
         v != NULL; v = g.get_next_vertex(c)) {
        buildRecur(v, g, domtree);
    }
}


//Count Dominator Frontier Density for each xcom::Vertex.
//Return true if there exist vertex that might inserting
//ton of phis which will blow up memory.
bool DfMgr::hasHighDFDensityVertex(xcom::DGraph const& g)
{
    Vector<UINT> counter_of_vex(g.get_vertex_num());
    INT c;
    for (xcom::Vertex const* v = g.get_first_vertex(c);
         v != NULL; v = g.get_next_vertex(c)) {
        xcom::BitSet const* dfset = getDFControlSet(VERTEX_id(v));
        if (dfset == NULL) { continue; }
        for (INT i = dfset->get_first(); i >= 0; i = dfset->get_next(i)) {
            UINT cc = counter_of_vex.get(i) + 1;
            if (cc >= m_thres) {
                return true;
            }
            counter_of_vex.set(i, cc);
        }
    }

    return false;
}


//This function compute dominance frontier to graph g.
void DfMgr::build(xcom::DGraph const& g)
{
    INT c;
    for (xcom::Vertex const* v = g.get_first_vertex(c);
         v != NULL; v = g.get_next_vertex(c)) {
        xcom::BitSet const* v_dom = g.read_dom_set(VERTEX_id(v));
        ASSERT0(v_dom != NULL);
        UINT vid = VERTEX_id(v);

        //Access each preds
        for (xcom::EdgeC const* ec = VERTEX_in_list(v);
             ec != NULL; ec = EC_next(ec)) {
            xcom::Vertex const* pred = EDGE_from(EC_edge(ec));
            xcom::BitSet * pred_df = genDFControlSet(VERTEX_id(pred));
            if (pred == v || g.get_idom(vid) != VERTEX_id(pred)) {
                pred_df->bunion(vid);
            }

            xcom::BitSet const* pred_dom = g.read_dom_set(VERTEX_id(pred));
            ASSERT0(pred_dom != NULL);
            for (INT i = pred_dom->get_first();
                 i >= 0; i = pred_dom->get_next(i)) {
                if (!v_dom->is_contain(i)) {
                    ASSERT0(g.get_vertex(i));
                    genDFControlSet(i)->bunion(vid);
                }
            }
        }
    }
}
//END DfMgr



//
//START SSAGraph
//
SSAGraph::SSAGraph(Region * rg, PRSSAMgr * ssamgr)
{
    ASSERT0(rg && ssamgr);
    m_ru = rg;
    m_ssa_mgr = ssamgr;
    Vector<VP*> const* vp_vec = ssamgr->getVPVec();
    UINT inputcount = 1;
    for (INT i = 1; i <= vp_vec->get_last_idx(); i++) {
        VP * v = vp_vec->get(i);
        ASSERT0(v != NULL);
        IR * def = SSA_def(v);
        if (def == NULL) {
            ASSERT0(VP_ver(v) == 0);
            UINT vdef = 0xffffFFFF - inputcount;
            inputcount++;
            addVertex(vdef);
            m_vdefs.set(vdef, v);

            //May be input parameters.
            SSAUseIter vit = NULL;
            for (INT i2 = SSA_uses(v).get_first(&vit);
                  vit != NULL; i2 = SSA_uses(v).get_next(i2, &vit)) {
                IR * use = m_ru->getIR(i2);
                ASSERT0(use->is_pr());
                addEdge(vdef, IR_id(use->getStmt()));
            }
        } else {
            ASSERT0(def->is_stmt());
            addVertex(def->id());
            SSAUseIter vit = NULL;
            for (INT i2 = SSA_uses(v).get_first(&vit);
                 vit != NULL; i2 = SSA_uses(v).get_next(i2, &vit)) {
                IR * use = m_ru->getIR(i2);
                ASSERT0(use->is_pr());
                addEdge(def->id(), IR_id(use->getStmt()));
            }
        }
    }
}


void SSAGraph::dump(CHAR const* name, bool detail)
{
    if (name == NULL) {
        name = "graph_ssa_graph.vcg";
    }
    UNLINK(name);
    FILE * h = fopen(name, "a+");
    ASSERTN(h != NULL, ("%s create failed!!!",name));

    //Print comment
    fprintf(h, "\n/*");
    FILE * old = g_tfile;
    g_tfile = h;
    dumpBBList(m_ru->getBBList(), m_ru);
    g_tfile = old;
    fprintf(h, "\n*/\n");

    //Print graph structure description.
    fprintf(h, "graph: {"
              "title: \"Graph\"\n"
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
              "layoutalgorithm: mindepthslow\n"
              "node.borderwidth: 2\n"
              "node.color: lightcyan\n"
              "node.textcolor: black\n"
              "node.bordercolor: blue\n"
              "edge.color: darkgreen\n");

    //Print Region name.
    fprintf(h,
            "\nnode: {title:\"\" shape:rhomboid color:turquoise "
            "borderwidth:0 fontname:\"Courier Bold\" "
            "scaling:2 label:\"Region:%s\" }", m_ru->getRegionName());

    //Print node
    old = g_tfile;
    g_tfile = h;
    List<IR const*> lst;
    INT c;
    for (xcom::Vertex * v = m_vertices.get_first(c);
         v != NULL; v = m_vertices.get_next(c)) {
        VP * vp = m_vdefs.get(VERTEX_id(v));
        if (vp != NULL) {
            //Print virtual def for parameter.
            fprintf(h,
                    "\nnode: { title:\"%u\" shape:hexagon fontname:\"courB\" "
                    "color:lightgrey label:\" param:P%uV%u \"}",
                    VERTEX_id(v), VP_prno(vp), VP_ver(vp));
            continue;
        }

        IR * def = m_ru->getIR(VERTEX_id(v));
        ASSERT0(def != NULL);
        IR * res = def->getResultPR();
        if (res != NULL) {
            fprintf(h, "\nnode: { title:\"%d\" shape:box fontname:\"courB\" "
                        "color:gold label:\"", def->id());
            for (IR * r = res; r != NULL; r = r->get_next()) {
                VP * vp2 = (VP*)r->getSSAInfo();
                fprintf(h, "P%uV%u ", VP_prno(vp2), VP_ver(vp2));
            }
            fprintf(h, " <-- ");
        } else {
            fprintf(h, "\nnode: { title:\"%u\" shape:box fontname:\"courB\" "
                        "color:gold label:\" <-- ",
                    def->id());
        }

        lst.clean();
        for (IR const* opnd = iterInitC(def, lst);
             opnd != NULL; opnd = iterNextC(lst)) {
             if (!def->is_rhs(opnd) || !opnd->is_pr()) {
                 continue;
             }
             VP * use_vp = (VP*)PR_ssainfo(opnd);
             fprintf(h, "pr%dv%d, ", VP_prno(use_vp), VP_ver(use_vp));
        }
        if (detail) {
            //TODO: implement dump_ir_buf();
            dumpIR(def, m_ru, NULL, IR_DUMP_KID);
        }
        fprintf(h, "\"}");
    }

    //Print edge
    for (xcom::Edge * e = m_edges.get_first(c);
         e != NULL; e = m_edges.get_next(c)) {
        xcom::Vertex * from = EDGE_from(e);
        xcom::Vertex * to = EDGE_to(e);
        fprintf(h, "\nedge: { sourcename:\"%u\" targetname:\"%u\" %s}",
                VERTEX_id(from), VERTEX_id(to),  "");
    }
    g_tfile = old;
    fprintf(h, "\n}\n");
    fclose(h);
}
//END SSAGraph



//
//START PRSSAMgr
//
size_t PRSSAMgr::count_mem()
{
    size_t count = 0;
    count += smpoolGetPoolSize(m_vp_pool);
    count += m_map_prno2vp_vec.count_mem();
    count += m_map_prno2stack.count_mem();
    count += sizeof(m_vp_count);
    count += m_vp_vec.count_mem();
    count += m_max_version.count_mem();
    count += m_prno2ir.count_mem();
    count += sizeof(m_ru);
    return count;
}


//Clean version stack.
void PRSSAMgr::cleanPRNO2Stack()
{
    for (INT i = 0; i <= m_map_prno2stack.get_last_idx(); i++) {
        Stack<VP*> * s = m_map_prno2stack.get(i);
        if (s != NULL) { delete s; }
    }
    m_map_prno2stack.clean();
}


//Dump ssa du stmt graph.
void PRSSAMgr::dumpSSAGraph(CHAR * name)
{
    SSAGraph sa(m_ru, this);
    sa.dump(name, true);
}


CHAR * PRSSAMgr::dumpVP(IN VP * v, OUT CHAR * buf)
{
    sprintf(buf, "P%dV%d", VP_prno(v), VP_ver(v));
    return buf;
}


//This function dumps VP structure and SSA DU info.
//have_renamed: set true if PRs have been renamed in construction.
void PRSSAMgr::dumpAllVP(bool have_renamed)
{
    if (g_tfile == NULL) { return; }
    note("\n==---- DUMP PRSSAMgr:VP_DU '%s' ----==\n", m_ru->getRegionName());

    Vector<VP*> const* vp_vec = getVPVec();
    for (INT i = 1; i <= vp_vec->get_last_idx(); i++) {
        VP * v = vp_vec->get(i);
        ASSERT0(v != NULL);
        note("\nid%d:pr%dver%d: ", SSA_id(v), VP_prno(v), VP_ver(v));
        IR * def = SSA_def(v);
        if (VP_ver(v) != 0 && !have_renamed) {
            //After renaming, version is meaningless.
            ASSERT0(def);
        }
        if (def != NULL) {
            ASSERT0(def->is_stmt());
            if (def->is_stpr()) {
                prt("DEF:st pr%d,id%d", def->getPrno(), def->id());
            } else if (def->is_phi()) {
                prt("DEF:phi pr%d,id%d", def->getPrno(), def->id());
            } else if (def->isCallStmt()) {
                prt("DEF:call");
                if (def->hasReturnValue()) {
                    prt(" pr%d,id%d", def->getPrno(), def->id());
                }
            } else {
                ASSERTN(0, ("not def stmt of PR"));
            }
        } else {
            prt("DEF:---");
        }

        prt("\tUSE:");
        SSAUseIter vit = NULL;
        INT nexti = 0;
        for (INT i2 = SSA_uses(v).get_first(&vit); vit != NULL; i2 = nexti) {
            nexti = SSA_uses(v).get_next(i2, &vit);
            IR * use = m_ru->getIR(i2);
            ASSERT0(use->is_pr());
            prt("(pr%d,id%d)", use->getPrno(), IR_id(use));
            if (nexti >= 0) {
                prt(",");
            }
        }
    }
    fflush(g_tfile);
}


void PRSSAMgr::dump()
{
    if (g_tfile == NULL) { return; }
    note("\n==---- DUMP Result of PRSSAMgr '%s' ----==\n",
        m_ru->getRegionName());

    BBList * bbl = m_ru->getBBList();
    List<IR const*> lst;
    List<IR const*> opnd_lst;
    for (IRBB * bb = bbl->get_head();
         bb != NULL; bb = bbl->get_next()) {
        note("\n--- BB%d ---", BB_id(bb));
        for (IR * ir = BB_first_ir(bb);
             ir != NULL; ir = BB_next_ir(bb)) {
            g_indent = 4;
            note("\n------------------");
            dumpIR(ir, m_ru);
            lst.clean();
            opnd_lst.clean();
            for (IR const* opnd = iterInitC(ir, lst);
                opnd != NULL;
                opnd = iterNextC(lst)) {
                if (ir->is_rhs(opnd) && opnd->is_pr()) {
                    opnd_lst.append_tail(opnd);
                }
            }

            IR * res = ir->getResultPR();
            note("\nVP:");
            if (res != NULL) {
                VP * vp = (VP*)res->getSSAInfo();
                ASSERT0(vp);
                prt("p%dv%d ", VP_prno(vp), VP_ver(vp));
            } else {
                prt("--");
            }
            prt(" <= ");
            if (opnd_lst.get_elem_count() != 0) {
                UINT i = 0, n = opnd_lst.get_elem_count() - 1;
                for (IR const* opnd = opnd_lst.get_head(); opnd != NULL;
                     opnd = opnd_lst.get_next(), i++) {
                    VP * vp = (VP*)PR_ssainfo(opnd);
                    ASSERT0(vp);
                    prt("p%dv%d", VP_prno(vp), VP_ver(vp));
                    if (i < n) { prt(","); }
                }
            } else {
                prt("--");
            }
        }
    }
    fflush(g_tfile);
}


//Initialize VP for each PR.
IR * PRSSAMgr::initVP(IN IR * ir)
{
    IR * prres = ir->getResultPR();

    //Process result.
    if (prres != NULL) {
        UINT prno = prres->getPrno();
        ir->setSSAInfo(allocVP(prno, 0));
        if (m_prno2ir.get(prno) == NULL) {
            m_prno2ir.set(prno, ir);
        }
    }

    //Process opnd.
    m_iter.clean();
    for (IR * kid = iterInit(ir, m_iter);
         kid != NULL; kid = iterNext(m_iter)) {
        if (ir->is_rhs(kid) && kid->is_pr()) {
            PR_ssainfo(kid) = allocVP(PR_no(kid), 0);
            if (m_prno2ir.get(PR_no(kid)) == NULL) {
                m_prno2ir.set(PR_no(kid), kid);
            }
        }
    }
    return prres;
}


//'mustdef_pr': record PRs which defined in 'bb'.
void PRSSAMgr::collectDefinedPR(IN IRBB * bb, OUT DefSBitSet & mustdef_pr)
{
    for (IR * ir = BB_first_ir(bb); ir != NULL; ir = BB_next_ir(bb)) {
        //Generate VP for non-phi stmt.
        IR * res = initVP(ir);
        for (IR * r = res; r != NULL; r = r->get_next()) {
            mustdef_pr.bunion(r->getPrno());
        }
    }
}


void PRSSAMgr::insertPhi(UINT prno, IN IRBB * bb)
{
    UINT num_opnd = m_cfg->get_in_degree(m_cfg->get_vertex(BB_id(bb)));
    IR * pr = m_prno2ir.get(prno);
    ASSERT0(pr);

    //Here each operand and result of phi set to same type.
    //They will be revised to correct type during renaming.
    IR * phi = m_ru->buildPhi(pr->getPrno(), pr->getType(), num_opnd);

    m_ru->allocRefForPR(phi);

    for (IR * opnd = PHI_opnd_list(phi); opnd != NULL; opnd = opnd->get_next()) {
        opnd->copyRef(phi, m_ru);
    }

    BB_irlist(bb).append_head(phi);

    initVP(phi);
}


//Insert phi for PR.
//defbbs: record BBs which defined the PR identified by 'prno'.
void PRSSAMgr::placePhiForPR(
        UINT prno,
        IN List<IRBB*> * defbbs,
        DfMgr const& dfm,
        xcom::BitSet & visited,
        List<IRBB*> & wl,
        Vector<DefSBitSet*> & defined_prs_vec)
{
    visited.clean();
    wl.clean();
    for (IRBB * defbb = defbbs->get_head();
         defbb != NULL; defbb = defbbs->get_next()) {
        wl.append_tail(defbb);
        //visited.bunion(BB_id(defbb));
    }

    while (wl.get_elem_count() != 0) {
        IRBB * bb = wl.remove_head();

        //Each BB in Set dfcs is in dominance frontier of 'bb'.
        xcom::BitSet const* dfcs = dfm.getDFControlSet(BB_id(bb));
        if (dfcs == NULL) { continue; }

        for (INT i = dfcs->get_first(); i >= 0; i = dfcs->get_next(i)) {
            if (visited.is_contain(i)) {
                //Already insert phi for 'prno' into BB i.
                //TODO:ensure the phi for same PR does NOT be
                //inserted multiple times.
                continue;
            }

            visited.bunion(i);

            IRBB * ibb = m_cfg->getBB(i);
            ASSERT0(ibb);

            //Redundant phi will be removed during refinePhi().
            insertPhi(prno, ibb);

            ASSERT0(defined_prs_vec.get(i));
            defined_prs_vec.get(i)->bunion(prno);

            wl.append_tail(ibb);
        }
    }
}


//Return true if phi is redundant, otherwise return false.
//If all opnds have same defintion or defined by current phi,
//the phi is redundant.
//common_def: record the common_def if the definition of all opnd is the same.
//TODO: p=phi(m,p), the only use of p is phi. the phi is redundant.
bool PRSSAMgr::isRedundantPHI(IR const* phi, OUT IR ** common_def) const
{
    ASSERT0(phi->is_phi());

    VP * vp = (VP*)PHI_ssainfo(phi);
    ASSERT0(vp);
    if (SSA_uses(vp).get_elem_count() == 0) { return true; }

    #define DUMMY_DEF_ADDR    0x1234
    IR * def = NULL;
    bool same_def = true; //indicate all DEF of operands are the same stmt.
    for (IR const* opnd = PHI_opnd_list(phi);
         opnd != NULL; opnd = opnd->get_next()) {
        ASSERT0(opnd->is_phi_opnd());

        if (!opnd->is_pr()) { continue; }

        SSAInfo const* si = PR_ssainfo(opnd);
        ASSERT0(si);

        if (SSA_def(si) != NULL) {
            if (def == NULL) {
                def = SSA_def(si);
            } else if (def != SSA_def(si) && def != phi) {
                same_def = false;
                break;
            }
        } else {
            //Assign def a dummy value to inidcate the region-live-in
            //PR(or so called argument).
            def = (IR*)DUMMY_DEF_ADDR;
        }
    }

    ASSERT0(common_def);
    if (def == (IR*)DUMMY_DEF_ADDR) {
        *common_def = NULL;
    } else {
        *common_def = def;
    }
    return same_def;
}


//Place phi and assign the v0 for each PR.
//'effect_prs': record the pr which need to versioning.
void PRSSAMgr::placePhi(DfMgr const& dfm,
                        OUT DefSBitSet & effect_prs,
                        DefMiscBitSetMgr & bs_mgr,
                        Vector<DefSBitSet*> & defined_prs_vec,
                        List<IRBB*> & wl)
{
    START_TIMER(t, "PRSSA: Place phi");

    //Record BBs which modified each PR.
    BBList * bblst = m_ru->getBBList();
    Vector<List<IRBB*>*> pr2defbb(bblst->get_elem_count()); //for local used.

    for (IRBB * bb = bblst->get_head(); bb != NULL; bb = bblst->get_next()) {
        DefSBitSet * bs = bs_mgr.allocSBitSet();
        defined_prs_vec.set(BB_id(bb), bs);
        collectDefinedPR(bb, *bs);

        //Regard all defined PR as effect, and they will be versioned later.
        effect_prs.bunion(*bs);

        //Record which BB defined these effect prs.
        SEGIter * cur = NULL;
        for (INT i = bs->get_first(&cur); i >= 0; i = bs->get_next(i, &cur)) {
            List<IRBB*> * bbs = pr2defbb.get(i);
            if (bbs == NULL) {
                bbs = new List<IRBB*>();
                pr2defbb.set(i, bbs);
            }
            bbs->append_tail(bb);
        }
    }

    //Place phi for lived effect prs.
    xcom::BitSet visited((bblst->get_elem_count()/8)+1);
    SEGIter * cur = NULL;
    for (INT i = effect_prs.get_first(&cur);
         i >= 0; i = effect_prs.get_next(i, &cur)) {
        placePhiForPR(i, pr2defbb.get(i), dfm, visited, wl, defined_prs_vec);
    }
    END_TIMER(t, "PRSSA: Place phi");

    //Free local used objects.
    for (INT i = 0; i <= pr2defbb.get_last_idx(); i++) {
        List<IRBB*> * bbs = pr2defbb.get(i);
        if (bbs == NULL) { continue; }
        delete bbs;
    }
}


//Rename vp from current version to the top-version on stack if it exist.
void PRSSAMgr::renameBB(IN IRBB * bb)
{
     for (IR * ir = BB_first_ir(bb); ir != NULL; ir = BB_next_ir(bb)) {
        if (!ir->is_phi()) {
            //Rename opnd, not include phi.
            //Walk through rhs expression IR tree to rename IR_PR's VP.
            m_iter.clean();
            for (IR * opnd = iterInit(ir, m_iter);
                 opnd != NULL; opnd = iterNext(m_iter)) {
                if (!ir->is_rhs(opnd) || !opnd->is_pr()) {
                    continue;
                }

                //Get the top-version on stack.
                Stack<VP*> * vs = mapPRNO2VPStack(PR_no(opnd));
                ASSERT0(vs);
                VP * topv = vs->get_top();
                if (topv == NULL) {
                    //prno has no top-version, it has no def, may be parameter.
                    ASSERT0(PR_ssainfo(opnd));
                    ASSERTN(VP_ver((VP*)PR_ssainfo(opnd)) == 0,
                           ("parameter only has first version"));
                    continue;
                }

                //e.g: pr1 = pr2(vp1)
                //    vp1 will be renamed to vp2, so vp1 does not
                //    occur in current IR any more.
                VP * curv = (VP*)PR_ssainfo(opnd);
                ASSERT0(curv && VP_prno(curv) == PR_no(opnd));

                //Set newest version VP uses current opnd.
                if (VP_ver(topv) == 0) {
                    //Each opnd only meet once.
                    ASSERT0(curv == topv);
                    ASSERT0(!SSA_uses(topv).find(opnd));
                    SSA_uses(topv).append(opnd);
                } else if (curv != topv) {
                    //curv may be ver0.
                    //Current ir does not refer the old version VP any more.
                    ASSERT0(VP_ver(curv) == 0 || SSA_uses(curv).find(opnd));
                    ASSERT0(VP_ver(curv) == 0 || SSA_def(curv) != NULL);
                    ASSERT0(!SSA_uses(topv).find(opnd));

                    SSA_uses(curv).remove(opnd);
                    PR_ssainfo(opnd) = topv;
                    SSA_uses(topv).append(opnd);
                }
            }
        }

        //Rename result, include phi.
        IR * res = ir->getResultPR();
        if (res != NULL) {
            ASSERT0(res->is_single());

            VP * resvp = (VP*)res->getSSAInfo();
            ASSERT0(resvp);
            ASSERT0(VP_prno(resvp) == res->getPrno());
            UINT prno = VP_prno(resvp);
            UINT maxv = m_max_version.get(prno);
            VP * new_v = allocVP(prno, maxv + 1);
            m_max_version.set(prno, maxv + 1);

            mapPRNO2VPStack(prno)->push(new_v);
            res->setSSAInfo(new_v);
            SSA_def(new_v) = ir;
        }
    }
}


Stack<VP*> * PRSSAMgr::mapPRNO2VPStack(UINT prno)
{
    Stack<VP*> * stack = m_map_prno2stack.get(prno);
    if (stack == NULL) {
        stack = new Stack<VP*>();
        m_map_prno2stack.set(prno, stack);
    }
    return stack;
}


void PRSSAMgr::handleBBRename(
        IRBB * bb,
        DefSBitSet const& defined_prs,
        IN OUT BB2VPMap & bb2vp)
{
    ASSERT0(bb2vp.get(BB_id(bb)) == NULL);
    TMap<UINT, VP*> * prno2vp = new TMap<UINT, VP*>();
    bb2vp.set(BB_id(bb), prno2vp);

    SEGIter * cur = NULL;
    for (INT prno = defined_prs.get_first(&cur);
         prno >= 0; prno = defined_prs.get_next(prno, &cur)) {
        VP * vp = mapPRNO2VPStack(prno)->get_top();
        ASSERT0(vp);
        prno2vp->set(VP_prno(vp), vp);
    }
    renameBB(bb);

    //Rename PHI opnd in successor BB.
    List<IRBB*> succs;
    m_cfg->get_succs(succs, bb);
    if (succs.get_elem_count() > 0) {
        //Replace the jth opnd of PHI with 'topv' which in bb's successor.
        List<IRBB*> preds;
        for (IRBB * succ = succs.get_head();
             succ != NULL; succ = succs.get_next()) {
            //Compute which predecessor 'bb' is with respect to its successor.
            m_cfg->get_preds(preds, succ);
            UINT idx = 0; //the index of corresponding predecessor.
            IRBB * p;
            for (p = preds.get_head();
                 p != NULL; p = preds.get_next(), idx++) {
                if (p == bb) {
                    break;
                }
            }
            ASSERT0(p != NULL);

            //Replace opnd of PHI of 'succ' with top SSA version.
            xcom::C<IR*> * ct;
            for (IR * ir = BB_irlist(succ).get_head(&ct);
                 ir != NULL; ir = BB_irlist(succ).get_next(&ct)) {
                if (!ir->is_phi()) {
                    break;
                }

                //Update version for same PR.
                IR * opnd = PHI_opnd_list(ir);
                UINT j = 0;
                while (opnd != NULL && j < idx) {
                    opnd = opnd->get_next();
                    j++;
                }
                ASSERT0(j == idx && opnd);
                VP * old = (VP*)PR_ssainfo(opnd);
                ASSERT0(old != NULL);
                VP * topv = mapPRNO2VPStack(VP_prno(old))->get_top();
                ASSERT0(topv != NULL);

                SSA_uses(old).remove(opnd);

                if (SSA_def(topv) != NULL) {
                    //topv might be zero version.
                    IR * defres = SSA_def(topv)->getResultPR(VP_prno(topv));
                    ASSERT0(defres);

                    IR * new_opnd = m_ru->buildPRdedicated(
                        defres->getPrno(), defres->getType());
                    new_opnd->copyRef(defres, m_ru);
                    replace(&PHI_opnd_list(ir), opnd, new_opnd);
                    IR_parent(new_opnd) = ir;
                    m_ru->freeIRTree(opnd);
                    opnd = new_opnd;

                    //Phi should have same type with opnd.
                    IR_dt(ir) = IR_dt(defres);
                }

                PR_ssainfo(opnd) = topv;

                //Add version0 opnd here, means opnd will be add to ver0
                //use-list if topv is version0. So one does not need to
                //add version0 at placePhi.
                SSA_uses(topv).append(opnd);
            }
        }
    }
}


//Linear renaming algorithm.
//defined_prs_vec: for each BB, indicate PRs which has been defined.
void PRSSAMgr::renameInDomTreeOrder(
        IRBB * root,
        xcom::Graph const& domtree,
        Vector<DefSBitSet*> const& defined_prs_vec)
{
    Stack<IRBB*> stk;
    UINT n = m_ru->getBBList()->get_elem_count();
    xcom::BitSet visited(n / BIT_PER_BYTE);
    BB2VPMap bb2vpmap(n);
    IRBB * v;
    stk.push(root);
    List<IR*> lst; //for tmp use.
    while ((v = stk.get_top()) != NULL) {
        if (!visited.is_contain(BB_id(v))) {
            visited.bunion(BB_id(v));
            DefSBitSet * defined_prs = defined_prs_vec.get(BB_id(v));
            ASSERT0(defined_prs);
            handleBBRename(v, *defined_prs, bb2vpmap);
        }

        xcom::Vertex const* bbv = domtree.get_vertex(BB_id(v));
        xcom::EdgeC const* c = VERTEX_out_list(bbv);
        bool all_visited = true;
        while (c != NULL) {
            xcom::Vertex * dom_succ = EDGE_to(EC_edge(c));
            if (dom_succ == bbv) { continue; }
            if (!visited.is_contain(VERTEX_id(dom_succ))) {
                ASSERT0(m_cfg->getBB(VERTEX_id(dom_succ)));
                all_visited = false;
                stk.push(m_cfg->getBB(VERTEX_id(dom_succ)));
                break;
            }
            c = EC_next(c);
        }

        if (all_visited) {
            stk.pop();

            //Do post-processing while all kids of BB has been processed.
            TMap<UINT, VP*> * prno2vp = bb2vpmap.get(BB_id(v));
            ASSERT0(prno2vp);
            DefSBitSet const* defined_prs = defined_prs_vec.get(BB_id(v));
            ASSERT0(defined_prs);

            SEGIter * cur = NULL;
            for (INT i = defined_prs->get_first(&cur);
                 i >= 0; i = defined_prs->get_next(i, &cur)) {
                Stack<VP*> * vs = mapPRNO2VPStack(i);
                ASSERT0(vs->get_bottom() != NULL);
                VP * vp = prno2vp->get(VP_prno(vs->get_top()));
                while (vs->get_top() != vp) {
                    vs->pop();
                }
            }

            //vpmap is useless from now on.
            bb2vpmap.set(BB_id(v), NULL);
            delete prno2vp;
        }
    }

    #ifdef _DEBUG_
    //Verify if vpmap of each BB has been deleted.
    for (INT i = 0; i <= bb2vpmap.get_last_idx(); i++) {
        ASSERT0(bb2vpmap.get(i) == NULL);
    }
    #endif
}


//Rename variables.
void PRSSAMgr::rename(
        DefSBitSet const& effect_prs,
        Vector<DefSBitSet*> const& defined_prs_vec,
        xcom::Graph const& domtree)
{
    START_TIMER(t, "PRSSA: Rename");
    if (m_ru->getBBList()->get_elem_count() == 0) { return; }
    SEGIter * cur = NULL;
    for (INT prno = effect_prs.get_first(&cur);
         prno >= 0; prno = effect_prs.get_next(prno, &cur)) {
        VP * vp = allocVP(prno, 0);
        mapPRNO2VPStack(prno)->push(vp);
    }

    ASSERT0(m_cfg->get_entry());
    renameInDomTreeOrder(m_cfg->get_entry(), domtree, defined_prs_vec);
    END_TIMER(t, "PRSSA: Rename");
}


void PRSSAMgr::destructBBSSAInfo(IRBB * bb)
{
    xcom::C<IR*> * ct, * next_ct;
    BB_irlist(bb).get_head(&next_ct);
    ct = next_ct;
    for (; ct != BB_irlist(bb).end(); ct = next_ct) {
        next_ct = BB_irlist(bb).get_next(next_ct);
        IR * ir = ct->val();
        if (!ir->is_phi()) { break; }

        stripPhi(ir, ct);
        BB_irlist(bb).remove(ct);
        m_ru->freeIRTree(ir);
    }
}


void PRSSAMgr::destructionInDomTreeOrder(IRBB * root, xcom::Graph & domtree)
{
    Stack<IRBB*> stk;
    UINT n = m_ru->getBBList()->get_elem_count();
    xcom::BitSet visited(n / BIT_PER_BYTE);
    BB2VPMap bb2vp(n);
    IRBB * v;
    stk.push(root);
    while ((v = stk.get_top()) != NULL) {
        if (!visited.is_contain(BB_id(v))) {
            visited.bunion(BB_id(v));
            destructBBSSAInfo(v);
        }

        xcom::Vertex * bbv = domtree.get_vertex(BB_id(v));
        ASSERTN(bbv, ("dom tree is invalid."));

        xcom::EdgeC * c = VERTEX_out_list(bbv);
        bool all_visited = true;
        while (c != NULL) {
            xcom::Vertex * dom_succ = EDGE_to(EC_edge(c));
            if (dom_succ == bbv) { continue; }
            if (!visited.is_contain(VERTEX_id(dom_succ))) {
                ASSERT0(m_cfg->getBB(VERTEX_id(dom_succ)));
                all_visited = false;
                stk.push(m_cfg->getBB(VERTEX_id(dom_succ)));
                break;
            }
            c = EC_next(c);
        }

        if (all_visited) {
            stk.pop();
            //Do post-processing while all kids of BB has been processed.
        }
    }
}


//This function perform SSA destruction via scanning BB in preorder
//traverse dominator tree.
//Return true if inserting copy at the head of fallthrough BB
//of current BB's predessor.
void PRSSAMgr::destruction(DomTree & domtree)
{
    START_TIMER(t, "PRSSA: destruction in dom tree order");

    BBList * bblst = m_ru->getBBList();
    if (bblst->get_elem_count() == 0) { return; }
    ASSERT0(m_cfg->get_entry());
    destructionInDomTreeOrder(m_cfg->get_entry(), domtree);
    cleanPRSSAInfo();
    m_is_ssa_constructed = false;

    END_TIMER(t, "PRSSA: destruction in dom tree order");
}


//Return true if inserting copy at the head of fallthrough BB
//of current BB's predessor.
//Note that do not free phi at this function, it will be freed
//by user.
void PRSSAMgr::stripPhi(IR * phi, xcom::C<IR*> * phict)
{
    IRBB * bb = phi->getBB();
    ASSERT0(bb);

    xcom::Vertex const* vex = m_cfg->get_vertex(BB_id(bb));
    ASSERT0(vex);

    //Temprarory RP to hold the result of PHI.
    IR * phicopy = m_ru->buildPR(phi->getType());
    phicopy->setRefMD(m_ru->genMDforPR(
        PR_no(phicopy), phicopy->getType()), m_ru);
    phicopy->cleanRefMDSet();
    IR * opnd = PHI_opnd_list(phi);

    //opnd may be CONST, LDA, PR.
    //ASSERT0(opnd->is_pr());
    ASSERT0(PHI_ssainfo(phi));

    UINT pos = 0;
    for (xcom::EdgeC * el = VERTEX_in_list(vex), * nextel = NULL;
         el != NULL; el = nextel, opnd = opnd->get_next(), pos++) {
        ASSERT0(find_position(VERTEX_in_list(vex), el) == pos);

        nextel = EC_next(el);
        INT pred = VERTEX_id(EDGE_from(EC_edge(el)));

        ASSERT0(opnd && opnd->is_exp());
        IR * opndcopy = m_ru->dupIRTree(opnd);
        if (!opndcopy->is_const()) {
            opndcopy->copyRefForTree(opnd, m_ru);
        }

        //The copy will be inserted into related predecessor.
        IR * store_to_phicopy = m_ru->buildStorePR(
            PR_no(phicopy), phicopy->getType(), opndcopy);
        store_to_phicopy->copyRef(phicopy, m_ru);

        IRBB * p = m_cfg->getBB(pred);
        ASSERT0(p);
        IR * plast = BB_last_ir(p);

        //In PHI node elimination to insert the copy in the predecessor block,
        //there is a check if last IR of BB is not a call then
        //place the copy there only.
        //However for call BB terminator, the copy will be placed at the start
        //of fallthrough BB.
        if (plast != NULL && plast->isCallStmt()) {
            IRBB * fallthrough = m_cfg->getFallThroughBB(p);
            if (!plast->is_terminate()) {
                //Fallthrough BB does not exist if the last ir is terminate.
                ASSERTN(fallthrough, ("invalid control flow graph."));
                if (BB_irlist(fallthrough).get_head() == NULL ||
                    !BB_irlist(fallthrough).get_head()->is_phi()) {
                    BB_irlist(fallthrough).append_head(store_to_phicopy);
                } else {
                    //Insert block to hold the copy.
                    IRBB * newbb = m_ru->allocBB();
                    m_ru->getBBList()->insert_after(newbb, p);
                    m_cfg->add_bb(newbb);
                    m_cfg->insertVertexBetween(
                        BB_id(p), BB_id(fallthrough), BB_id(newbb));
                    BB_is_fallthrough(newbb) = true;

                    //Then append the copy.
                    BB_irlist(newbb).append_head(store_to_phicopy);
                }
            }
        } else {
            BB_irlist(p).append_tail_ex(store_to_phicopy);
        }

        //Remove the SSA DU chain between opnd and its DEF stmt.
        if (opnd->is_pr()) {
            ASSERT0(PR_ssainfo(opnd));
            SSA_uses(PR_ssainfo(opnd)).remove(opnd);
        }
    }

    IR * substitue_phi = m_ru->buildStorePR(
        PHI_prno(phi), phi->getType(), phicopy);
    substitue_phi->copyRef(phi, m_ru);

    BB_irlist(bb).insert_before(substitue_phi, phict);

    PHI_ssainfo(phi) = NULL;
}


//This function verify def/use information of PHI stmt.
//If vpinfo is available, the function also check VP_prno of phi operands.
//is_vpinfo_avail: set true if VP information is available.
bool PRSSAMgr::verifyPhi(bool is_vpinfo_avail)
{
    DUMMYUSE(is_vpinfo_avail);
    BBList * bblst = m_ru->getBBList();
    List<IRBB*> preds;
    for (IRBB * bb = bblst->get_head(); bb != NULL; bb = bblst->get_next()) {
        m_cfg->get_preds(preds, bb);
        xcom::C<IR*> * ct;
        for (BB_irlist(bb).get_head(&ct); ct != BB_irlist(bb).end();
             ct = BB_irlist(bb).get_next(ct)) {
            IR const* ir = ct->val();
            if (!ir->is_phi()) { continue; }

            //Check phi result.
            VP * resvp = (VP*)PHI_ssainfo(ir);
            ASSERTN(!is_vpinfo_avail || VP_prno(resvp) == PHI_prno(ir),
                   ("prno unmatch"));

            //Check the number of phi opnds.
            UINT num_opnd = 0;

            for (IR const* opnd = PHI_opnd_list(ir);
                 opnd != NULL; opnd = opnd->get_next()) {
                //Opnd may be PR, CONST or LDA.
                ASSERTN(!is_vpinfo_avail ||
                       VP_prno((VP*)PR_ssainfo(opnd)) == PR_no(opnd),
                       ("prno of VP is unmatched"));

                //Ver0 is input parameter, and it has no SSA_def.
                //ASSERT0(VP_ver(PR_ssainfo(opnd)) > 0);

                num_opnd++;
            }

            ASSERTN(num_opnd == preds.get_elem_count(),
                  ("The number of phi operand must same with "
                   "the number of BB predecessors."));

            //Check SSA uses.
            SSAUseIter vit = NULL;
            for (INT i = SSA_uses(resvp).get_first(&vit);
                 vit != NULL; i = SSA_uses(resvp).get_next(i, &vit)) {
                IR * use = m_ru->getIR(i);
                ASSERT0(use->is_pr());

                ASSERTN(PR_no(use) == PHI_prno(ir), ("prno is unmatch"));

                SSAInfo * use_ssainfo = PR_ssainfo(use);
                CHECK_DUMMYUSE(use_ssainfo);

                ASSERT0(SSA_def(use_ssainfo) == ir);
            }
        }
    }
    return true;
}


//Check the consistency for IR_PR if VP_prno == PR_no.
//This function only can be invoked immediately
//after rename(), because refinePhi() might clobber VP information, that leads
//VP_prno() to be invalid.
bool PRSSAMgr::verifyPRNOofVP()
{
    ConstIRIter ii;
    BBList * bblst = m_ru->getBBList();
    xcom::C<IRBB*> * ct;
    for (IRBB * bb = bblst->get_head(&ct);
         bb != NULL; bb = bblst->get_next(&ct)) {
         for (IR * ir = BB_first_ir(bb); ir != NULL; ir = BB_next_ir(bb)) {
            ii.clean();
            for (IR const* opnd = iterInitC(ir, ii);
                 opnd != NULL; opnd = iterNextC(ii)) {
                if (opnd->is_pr()) {
                    ASSERT0(PR_no(opnd) == VP_prno((VP*)PR_ssainfo(opnd)));
                }
            }
         }
     }
    return true;
}


bool PRSSAMgr::verifyVP()
{
    //Check version for each vp.
    xcom::BitSet defset;
    for (INT i = 1; i <= m_vp_vec.get_last_idx(); i++) {
        VP * v = m_vp_vec.get(i);
        ASSERT0(v != NULL);
        IR * def = SSA_def(v);
        if (def == NULL) {
            //ver0 used to indicate the Region live-in PR. It may be a parameter.
            ASSERTN(VP_ver(v) == 0, ("Nondef vp's version must be 0"));
        } else {
            ASSERTN(VP_ver(v) != 0, ("version can not be 0"));
            ASSERT0(def->is_stmt());

            ASSERTN(!defset.is_contain(def->id()),
                    ("DEF for each pr+version must be unique."));
            defset.bunion(def->id());
        }

        IR const* respr = NULL;
        UINT defprno = 0;
        if (def != NULL) {
            respr = def->getResultPR(VP_prno(v));
            ASSERT0(respr);

            defprno = respr->getPrno();
            ASSERT0(defprno != PRNO_UNDEF);
        }

        SSAUseIter vit = NULL;
        UINT opndprno = 0;
        for (INT i2 = SSA_uses(v).get_first(&vit);
             vit != NULL; i2 = SSA_uses(v).get_next(i2, &vit)) {
            IR * use = m_ru->getIR(i2);

            ASSERT0(use->is_pr() || use->is_const());

            if (use->is_pr()) {
                if (opndprno == 0) {
                    opndprno = PR_no(use);
                } else {
                    //All opnd should have same PR no.
                    ASSERT0(opndprno == PR_no(use));
                }

                //Each USE of current VP must be defined by same stmt.
                ASSERT0(PR_ssainfo(use) == v);
            }
        }

        if (opndprno != 0 && defprno != 0) {
            //Def should have same PR no with USE.
            ASSERT0(opndprno == defprno);
        }
    }
    return true;
}


static void verify_ssainfo_core(IR * ir, xcom::BitSet & defset, Region * rg)
{
    ASSERT0(ir);
    SSAInfo * ssainfo = ir->getSSAInfo();
    ASSERTN(ssainfo, ("%s miss SSA info.", IRNAME(ir)));

    IR * def = SSA_def(ssainfo);

    if (ir->is_stmt()) {
        ASSERTN(def == ir, ("ir does not have SSA du"));
        ASSERTN(!defset.is_contain(ir->id()),
                ("DEF for each pr+version must be unique."));
        defset.bunion(def->id());
    }

    IR const* respr = NULL;
    UINT defprno = 0;
    if (def != NULL) {
        ASSERT0(def->is_stmt());
        respr = def->getResultPR();
        ASSERT0(respr);

        defprno = respr->getPrno();
        ASSERT0(defprno != PRNO_UNDEF);
    }

    SSAUseIter vit = NULL;
    UINT opndprno = 0;
    for (INT i = SSA_uses(ssainfo).get_first(&vit);
         vit != NULL; i = SSA_uses(ssainfo).get_next(i, &vit)) {
        IR * use = rg->getIR(i);

        ASSERT0(use->is_pr() || use->is_const() || use->is_lda());

        if (use->is_pr()) {
            if (opndprno == 0) {
                opndprno = PR_no(use);
            } else {
                //All opnd should have same PR no.
                ASSERT0(opndprno == PR_no(use));
            }

            //Each USE of current SSAInfo must be defined by same stmt.
            ASSERT0(PR_ssainfo(use) == ssainfo);
        }
    }

    if (opndprno != 0 && defprno != 0) {
        //Def should have same PR no with USE.
        ASSERT0(opndprno == defprno);
    }
}


//The verification check the DU info in SSA form.
//Current IR must be in SSA form.
bool PRSSAMgr::verifySSAInfo()
{
    //Check version for each vp.
    xcom::BitSet defset;
    BBList * bbl = m_ru->getBBList();
    xcom::C<IRBB*> * ct;
    for (bbl->get_head(&ct); ct != bbl->end(); ct = bbl->get_next(ct)) {
        IRBB * bb = ct->val();
        xcom::C<IR*> * ctir;
        for (BB_irlist(bb).get_head(&ctir);
             ctir != BB_irlist(bb).end();
             ctir = BB_irlist(bb).get_next(ctir)) {
            IR * ir = ctir->val();
            m_iter.clean();
            for (IR * x = iterInit(ir, m_iter);
                 x != NULL; x = iterNext(m_iter)) {
                if (x->isReadPR() ||
                    x->isWritePR() ||
                    x->isCallHasRetVal()) {
                    verify_ssainfo_core(x, defset, m_ru);
                }
            }
        }
    }
    return true;
}


//This function perform SSA destruction via scanning BB in sequential order.
void PRSSAMgr::destruction()
{
    BBList * bblst = m_ru->getBBList();
    if (bblst->get_elem_count() == 0) { return; }
    xcom::C<IRBB*> * bbct;
    for (bblst->get_head(&bbct);
         bbct != bblst->end(); bbct = bblst->get_next(bbct)) {
        IRBB * bb = bbct->val();
        ASSERT0(bb);
        destructBBSSAInfo(bb);
    }

    //Clean SSA info to avoid unnecessary abort or assert.
    cleanPRSSAInfo();
    m_is_ssa_constructed = false;
}


//Set SSAInfo of IR to be NULL to inform optimizer that IR is not in SSA form.
void PRSSAMgr::cleanPRSSAInfo()
{
    BBList * bblst = m_ru->getBBList();
    xcom::C<IRBB*> * bbct;
    for (bblst->get_head(&bbct);
         bbct != bblst->end(); bbct = bblst->get_next(bbct)) {
        IRBB * bb = bbct->val();
        ASSERT0(bb);

        xcom::C<IR*> * irct;
        for (BB_irlist(bb).get_head(&irct);
             irct != BB_irlist(bb).end();
             irct = BB_irlist(bb).get_next(irct)) {
            IR * ir = irct->val();
            ASSERT0(ir);

            m_iter.clean();
            for (IR * x = iterInit(ir, m_iter);
                 x != NULL; x = iterNext(m_iter)) {
                ASSERTN(!x->is_phi(), ("phi should have been striped."));

                if (x->isReadPR() ||
                    x->isWritePR() ||
                    x->isCallHasRetVal()) {
                    x->setSSAInfo(NULL);
                }
            }
        }
    }
}


static void revisePhiDataType(IR * phi, Region * rg)
{
    ASSERT0(phi->is_phi());
    //The data type of phi is set to be same type as its USE.
    SSAInfo * irssainfo = PHI_ssainfo(phi);
    ASSERT0(irssainfo);
    ASSERTN(SSA_uses(irssainfo).get_elem_count() > 0,
           ("phi has no use, it is redundant at all."));

    SSAUseIter si = NULL;
    INT i = SSA_uses(irssainfo).get_first(&si);
    ASSERT0(si && i >= 0);
    ASSERT0(rg->getIR(i)->is_pr());
    ASSERT0(PR_no(rg->getIR(i)) == PHI_prno(phi));

    IR_dt(phi) = rg->getIR(i)->getType();
}


//This function revise phi data type, and remove redundant phi.
//wl: work list for temporary used.
void PRSSAMgr::refinePhi(List<IRBB*> & wl)
{
    START_TIMER(t, "PRSSA: Refine phi");

    BBList * bblst = m_ru->getBBList();
    xcom::C<IRBB*> * ct;

    wl.clean();
    //Estimate memory usage.
    BitSet in_list(bblst->get_elem_count() / BITS_PER_BYTE / 4);
    for (bblst->get_head(&ct); ct != bblst->end(); ct = bblst->get_next(ct)) {
        IRBB * bb = ct->val();
        ASSERT0(bb);
        wl.append_tail(bb);
        in_list.bunion(bb->id());
    }

    IRBB * bb = NULL;
    while ((bb = wl.remove_head()) != NULL) {
        in_list.diff(bb->id());
        xcom::C<IR*> * irct = NULL;
        xcom::C<IR*> * nextirct = NULL;
        for (BB_irlist(bb).get_head(&nextirct), irct = nextirct;
             irct != BB_irlist(bb).end(); irct = nextirct) {
            nextirct = BB_irlist(bb).get_next(nextirct);
            IR * ir = irct->val();
            if (!ir->is_phi()) { break; }

            IR * common_def = NULL;
            if (isRedundantPHI(ir, &common_def)) {
                for (IR const* opnd = PHI_opnd_list(ir);
                     opnd != NULL; opnd = opnd->get_next()) {
                    ASSERT0(opnd->is_phi_opnd());

                    if (!opnd->is_pr()) { continue; }

                    SSAInfo * si = PR_ssainfo(opnd);
                    ASSERTN(si, ("Miss SSAInfo."));

                    SSA_uses(si).remove(opnd);

                    if (SSA_def(si) == NULL || !SSA_def(si)->is_phi()) {
                        continue;
                    }

                    IRBB * defbb = SSA_def(si)->getBB();
                    ASSERTN(defbb, ("defbb does not belong to any BB"));
                    wl.append_tail(defbb);
                    if (!in_list.is_contain(defbb->id())) {
                        wl.append_tail(defbb);
                        in_list.bunion(defbb->id());
                    }
                }

                SSAInfo * curphi_ssainfo = PHI_ssainfo(ir);
                ASSERT0(curphi_ssainfo);
                ASSERT0(SSA_def(curphi_ssainfo) == ir);

                if (common_def != NULL && ir != common_def) {
                    //All operands of PHI are defined by same alternative stmt,
                    //just call it common_def. Replace the SSA_def of
                    //current SSAInfo to the common_def.

                    ASSERT0(common_def->getResultPR(PHI_prno(ir)));
                    ASSERTN(common_def->getResultPR(PHI_prno(ir))->getPrno()
                            == PHI_prno(ir), ("not same PR"));

                    IR * respr = common_def->getResultPR(PHI_prno(ir));
                    ASSERT0(respr);

                    SSAInfo * commdef_ssainfo = respr->getSSAInfo();
                    ASSERT0(commdef_ssainfo);

                    SSA_uses(commdef_ssainfo).bunion(SSA_uses(curphi_ssainfo));

                    SSAUseIter si = NULL;
                    for (INT i = SSA_uses(curphi_ssainfo).get_first(&si);
                         si != NULL;
                         i = SSA_uses(curphi_ssainfo).get_next(i, &si)) {
                        IR * use = m_ru->getIR(i);
                        ASSERT0(use->is_pr());

                        ASSERT0(PR_ssainfo(use) &&
                                 PR_ssainfo(use) == curphi_ssainfo);

                        PR_ssainfo(use) = commdef_ssainfo;
                    }
                }

                ((VP*)curphi_ssainfo)->cleanMember();
                curphi_ssainfo->cleanDU();

                BB_irlist(bb).remove(irct);

                m_ru->freeIR(ir);

                continue;
            }

            revisePhiDataType(ir, m_ru);
        }
    }

    END_TIMER(t, "PRSSA: Refine phi");
}


//This function revise phi data type, and remove redundant phi.
void PRSSAMgr::stripVersionForBBList()
{
    START_TIMER(t, "PRSSA: Strip version");

    BBList * bblst = m_ru->getBBList();
    if (bblst->get_elem_count() == 0) { return; }

    xcom::C<IRBB*> * ct;

    xcom::BitSet visited;

    //Ensure the first allocation of bitset could
    //accommodata the last vp id.
    visited.bunion(m_vp_count);
    visited.diff(m_vp_count);

    for (bblst->get_head(&ct); ct != bblst->end(); ct = bblst->get_next(ct)) {
        IRBB * bb = ct->val();
        xcom::C<IR*> * irct = NULL;
        xcom::C<IR*> * nextirct;
        for (BB_irlist(bb).get_head(&nextirct), irct = nextirct;
             irct != BB_irlist(bb).end(); irct = nextirct) {
            nextirct = BB_irlist(bb).get_next(nextirct);

            IR * ir = irct->val();
            stripStmtVersion(ir, visited);
        }
    }

    END_TIMER(t, "PRSSA: Strip version");
}


//Return the replaced one.
static IR * replace_res_pr(
        IR * stmt,
        UINT oldprno,
        UINT newprno,
        Type const* newprty)
{
    DUMMYUSE(oldprno);
    //newprty may be VOID.
    ASSERT0(newprno != PRNO_UNDEF);

    //Replace stmt PR and DATA_TYPE info.
    ASSERT0(stmt->getPrno() == oldprno);
    stmt->setPrno(newprno);
    IR_dt(stmt) = newprty;
    return NULL;
}


//Strip specified VP's version.
void PRSSAMgr::stripSpecifiedVP(VP * vp)
{
    IR * def = SSA_def(vp);
    if (def == NULL) { return; }

    ASSERT0(VP_ver(vp) != 0);

    IR * res = def->getResultPR(VP_prno(vp));
    ASSERTN(res, ("Stmt does not modified PR%d", VP_prno(vp)));

    Type const* newprty = res->getType();
    UINT newprno = m_ru->buildPrno(newprty);

    IR * replaced_one = replace_res_pr(def, VP_prno(vp), newprno, newprty);
    ASSERT0(replaced_one);

    MD const* md = m_ru->genMDforPR(newprno, newprty);
    replaced_one->setRefMD(md, m_ru);
    if (replaced_one->isCallStmt()) {
        //Call stmts may have sideeffect modify MDSet.
        replaced_one->removePROutFromUseset(*m_ru->getMiscBitSetMgr(), m_ru);
    } else {
        def->freeDUset(*m_ru->getMiscBitSetMgr());
        replaced_one->cleanRefMDSet();
    }

    SSAUseIter vit = NULL;
    for (INT i = SSA_uses(vp).get_first(&vit);
         vit != NULL; i = SSA_uses(vp).get_next(i, &vit)) {
        IR * use = m_ru->getIR(i);
        ASSERT0(use->is_pr());

        PR_no(use) = newprno;

        //Keep the data type of reference unchanged.
        //IR_dt(use) = newprty;
        use->setRefMD(md, m_ru);

        //MD du is useless. Free it for other use.
        use->freeDUset(*m_ru->getMiscBitSetMgr());
    }

    //Set VP prno to the new prno to avoid verify_vp() assert.
    //However, checking VP_prno after strip_version, I think, is dispensable.
    VP_prno(vp) = newprno;
}


void PRSSAMgr::stripStmtVersion(IR * stmt, xcom::BitSet & visited)
{
    ASSERT0(stmt->is_stmt());

    if (stmt->isWritePR() || stmt->isCallHasRetVal()) {
        VP * vp = (VP*)stmt->getSSAInfo();
        ASSERT0(vp);

        if (!visited.is_contain(SSA_id(vp))) {
            ASSERT0(VP_ver(vp) != 0);

            //Avoid restriping again.
            visited.bunion(SSA_id(vp));

            stripSpecifiedVP(vp);
        }
    }

    //Process operand.
    m_iter.clean();
    for (IR * k = iterRhsInit(stmt, m_iter);
         k != NULL; k = iterRhsNext(m_iter)) {
        if (!k->is_pr()) { continue; }

        VP * vp = (VP*)k->getSSAInfo();
        ASSERT0(vp);

        if (!visited.is_contain(SSA_id(vp))) {
            //Version may be zero if there is not any DEF for k.
            //ASSERT0(VP_ver(vp) != 0);

            //MD du is useless. Free it for other use.
            k->freeDUset(*m_ru->getMiscBitSetMgr());

            //Avoid restriping again.
            visited.bunion(SSA_id(vp));

            stripSpecifiedVP(vp);
        }
    }
}


//Do striping for all VP recorded.
//We do not try to strip version for all VP, because the information of VP
//during striping will not be maintained and the relationship between
//VP_prno and the concrete occurrence PR may be invalid and
//that making the process assert.
void PRSSAMgr::stripVersionForAllVP()
{
    for (INT i = 1; i <= m_vp_vec.get_last_idx(); i++) {
        VP * v = m_vp_vec.get(i);
        ASSERT0(v != NULL);
        stripSpecifiedVP(v);
    }
}


//Construct DU chain which need by IR_DU_MGR.
//This function will build the DUSet for PHI and its USE.
void PRSSAMgr::constructMDDUChainForPR()
{
    for (INT i = 1; i <= m_vp_vec.get_last_idx(); i++) {
        VP * v = m_vp_vec.get(i);
        ASSERT0(v != NULL);
        IR * def = SSA_def(v);
        if (def == NULL) { continue; }

        ASSERT0(def->is_stmt());

        SSAUseIter vit = NULL;
        for (INT i2 = SSA_uses(v).get_first(&vit);
             vit != NULL; i2 = SSA_uses(v).get_next(i2, &vit)) {
            IR * use = m_ru->getIR(i2);
            ASSERT0(use->is_pr());
            ASSERT0(def->isExactDef(use->getRefMD()));
            m_ru->getDUMgr()->buildDUChain(def, use);
        }
    }
}


//Compute SSAInfo for IRs in region that are in SSA mode.
void PRSSAMgr::computeSSAInfo()
{
    reinit();
    BBList * bblst = m_ru->getBBList();
    ASSERT0(bblst);
    if (bblst->get_elem_count() == 0) { return; }

    IRIter ii;
    xcom::C<IRBB*> * bbct = NULL;
    for (bblst->get_head(&bbct); bbct != bblst->end(); bblst->get_next(&bbct)) {
        IRBB * bb = bbct->val();
        xcom::C<IR*> * irct = NULL;
        for (BB_irlist(bb).get_head(&irct);
             irct != BB_irlist(bb).end(); irct = BB_irlist(bb).get_next(irct)) {
            IR * ir = irct->val();
            ii.clean();
            for (IR * x = iterInit(ir, ii); x != NULL; x = iterNext(ii)) {
                if (x->isReadPR() || (x->is_stmt() && x->getResultPR() != NULL)) {
                    SSAInfo * ssainfo = allocSSAInfo(x->getPrno());
                    x->setSSAInfo(ssainfo);
                    if (x->isReadPR()) {
                        SSA_uses(ssainfo).append(x);
                    } else {
                        ASSERTN(SSA_def(ssainfo) == NULL,
                            ("multidefinition in for PR%d", x->getPrno()));
                        SSA_def(ssainfo) = x;
                    }
                }
            }
        }
    }
    m_is_ssa_constructed = true;
}


void PRSSAMgr::construction(OptCtx & oc)
{
    reinit();
    m_ru->checkValidAndRecompute(&oc, PASS_DOM, PASS_UNDEF);

    //Extract dominate tree of CFG.
    START_TIMER(t, "PRSSA: Extract Dom Tree");
    DomTree domtree;
    m_cfg->get_dom_tree(domtree);
    END_TIMER(t, "PRSSA: Extract Dom Tree");

    if (!construction(domtree)) {
        return;
    }

    OC_is_du_chain_valid(oc) = false; //DU chain of PR is voilated.
    m_is_ssa_constructed = true;
}


//Note: Non-SSA DU Chains of read/write PR will be clean and
//unusable after SSA construction.
bool PRSSAMgr::construction(DomTree & domtree)
{
    ASSERT0(m_ru);

    START_TIMER(t, "PRSSA: Build dominance frontier");
    DfMgr dfm;
    dfm.build((xcom::DGraph&)*m_cfg);

    END_TIMER(t, "PRSSA: Build dominance frontier");
    if (dfm.hasHighDFDensityVertex((xcom::DGraph&)*m_cfg)) {
        return false;
    }

    List<IRBB*> wl;
    DefMiscBitSetMgr sm;
    DefSBitSet effect_prs(sm.getSegMgr());
    Vector<DefSBitSet*> defined_prs_vec;

    placePhi(dfm, effect_prs, sm, defined_prs_vec, wl);

    rename(effect_prs, defined_prs_vec, domtree);

    ASSERT0(verifyPhi(true) && verifyPRNOofVP());

    refinePhi(wl);

    //Clean version stack after renaming.
    cleanPRNO2Stack();

    //Recompute the map if ssa needs reconstruct.
    m_prno2ir.clean();

    stripVersionForBBList();

    if (g_is_dump_after_pass) {
        START_TIMER(tdump, "PRSSA: Dump After Pass");
        dump();
        dfm.dump((xcom::DGraph&)*m_cfg);
        END_TIMER(tdump, "PRSSA: Dump After Pass");
    }

    ASSERT0(verifyIRandBB(m_ru->getBBList(), m_ru));
    ASSERT0(verifyPhi(false) && verifyVP());
    m_is_ssa_constructed = true;
    return true;
}
//END PRSSAMgr


bool verifySSAInfo(Region * rg)
{
    PRSSAMgr * ssamgr =
        (PRSSAMgr*)(rg->getPassMgr()->queryPass(PASS_PR_SSA_MGR));
    if (ssamgr != NULL && ssamgr->isSSAConstructed()) {
        ASSERT0(ssamgr->verifySSAInfo());
        ASSERT0(ssamgr->verifyPhi(false));
    }
    return true;
}

} //namespace xoc
