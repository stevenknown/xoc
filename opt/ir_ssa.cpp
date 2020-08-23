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
void DfMgr::dump(xcom::DGraph const& g) const
{
    if (g_tfile == NULL) { return; }
    note("\n==---- DUMP Dominator Frontier Control Set ----==\n");
    INT c;
    for (xcom::Vertex const* v = g.get_first_vertex(c);
         v != NULL; v = g.get_next_vertex(c)) {
        UINT vid = v->id();
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
void DfMgr::buildRecur(xcom::Vertex const* v,
                       xcom::DGraph const& g,
                       DomTree const& domtree)
{
    UINT vid = v->id();
    xcom::Vertex * v_domtree = domtree.getVertex(vid);
    ASSERT0(v_domtree);

    //Access each succs.
    for (xcom::EdgeC const* ec = VERTEX_out_list(v_domtree);
         ec != NULL; ec = EC_next(ec)) {
        xcom::Vertex const* succ_domtree = ec->getTo();
        xcom::Vertex const* succ = g.getVertex(succ_domtree->id());
        buildRecur(succ, g, domtree);
    }

    xcom::BitSet * df = genDFControlSet(vid);
    df->clean();

    //Compute DF(local)
    for (xcom::EdgeC const* ec = VERTEX_out_list(v);
         ec != NULL; ec = EC_next(ec)) {
        xcom::Vertex const* succ = ec->getTo();
        if (g.get_idom(succ->id()) != vid) {
            df->bunion(succ->id());
        }
    }

    //Compute DF(up)
    for (xcom::EdgeC const* ec = VERTEX_out_list(v_domtree);
         ec != NULL; ec = EC_next(ec)) {
        xcom::Vertex const* succ_domtree = ec->getTo();
        xcom::BitSet * succ_df = genDFControlSet(succ_domtree->id());
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
    Vector<UINT> counter_of_vex(g.getVertexNum());
    INT c;
    for (xcom::Vertex const* v = g.get_first_vertex(c);
         v != NULL; v = g.get_next_vertex(c)) {
        xcom::BitSet const* dfset = getDFControlSet(v->id());
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
        xcom::BitSet const* v_dom = g.read_dom_set(v->id());
        ASSERT0(v_dom != NULL);
        UINT vid = v->id();

        //Access each preds
        for (xcom::EdgeC const* ec = VERTEX_in_list(v);
             ec != NULL; ec = EC_next(ec)) {
            xcom::Vertex const* pred = ec->getFrom();
            xcom::BitSet * pred_df = genDFControlSet(pred->id());
            if (pred == v || g.get_idom(vid) != pred->id()) {
                pred_df->bunion(vid);
            }

            xcom::BitSet const* pred_dom = g.read_dom_set(pred->id());
            ASSERT0(pred_dom != NULL);
            for (INT i = pred_dom->get_first();
                 i >= 0; i = pred_dom->get_next(i)) {
                if (!v_dom->is_contain(i)) {
                    ASSERT0(g.getVertex(i));
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
    m_rg = rg;
    m_ssa_mgr = ssamgr;
    VPRVec const* vpr_vec = ssamgr->getVPRVec();
    UINT inputcount = 1;
    for (INT i = 1; i <= vpr_vec->get_last_idx(); i++) {
        VPR * v = vpr_vec->get(i);
        ASSERT0(v != NULL);
        IR * def = SSA_def(v);
        if (def == NULL) {
            ASSERT0(VPR_version(v) == 0);
            UINT vdef = 0xffffFFFF - inputcount;
            inputcount++;
            addVertex(vdef);
            m_vdefs.set(vdef, v);

            //May be input parameters.
            SSAUseIter vit = NULL;
            for (INT i2 = SSA_uses(v).get_first(&vit);
                  vit != NULL; i2 = SSA_uses(v).get_next(i2, &vit)) {
                IR * use = m_rg->getIR(i2);
                ASSERT0(use->is_pr());
                addEdge(vdef, IR_id(use->getStmt()));
            }
        } else {
            ASSERT0(def->is_stmt());
            addVertex(def->id());
            SSAUseIter vit = NULL;
            for (INT i2 = SSA_uses(v).get_first(&vit);
                 vit != NULL; i2 = SSA_uses(v).get_next(i2, &vit)) {
                IR * use = m_rg->getIR(i2);
                ASSERT0(use->is_pr());
                addEdge(def->id(), IR_id(use->getStmt()));
            }
        }
    }
}


void SSAGraph::dump(CHAR const* name, bool detail) const
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
    dumpBBList(m_rg->getBBList(), m_rg);
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
            "scaling:2 label:\"Region:%s\" }", m_rg->getRegionName());

    //Print node
    old = g_tfile;
    g_tfile = h;
    List<IR const*> lst;
    INT c;
    for (xcom::Vertex * v = m_vertices.get_first(c);
         v != NULL; v = m_vertices.get_next(c)) {
        VPR * vp = m_vdefs.get(v->id());
        if (vp != NULL) {
            //Print virtual def for parameter.
            fprintf(h,
                    "\nnode: { title:\"%u\" shape:hexagon fontname:\"courB\" "
                    "color:lightgrey label:\" param:P%uV%uP%u \"}",
                    v->id(), vp->orgprno(), vp->version(), vp->newprno());
            continue;
        }

        IR * def = m_rg->getIR(v->id());
        ASSERT0(def != NULL);
        IR * res = def->getResultPR();
        if (res != NULL) {
            fprintf(h, "\nnode: { title:\"%d\" shape:box fontname:\"courB\" "
                        "color:gold label:\"", def->id());
            for (IR * r = res; r != NULL; r = r->get_next()) {
                VPR * vp2 = (VPR*)r->getSSAInfo();
                fprintf(h, "P%uV%uP%u ", vp2->orgprno(),
                        vp2->version(), vp2->newprno());
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
             VPR * use_vp = (VPR*)PR_ssainfo(opnd);
             fprintf(h, "$%dv%d$%d, ", use_vp->orgprno(),
                     use_vp->version(), use_vp->newprno());
        }
        if (detail) {
            //TODO: implement dump_ir_buf();
            dumpIR(def, m_rg, NULL, IR_DUMP_KID);
        }
        fprintf(h, "\"}");
    }

    //Print edge
    for (xcom::Edge * e = m_edges.get_first(c);
         e != NULL; e = m_edges.get_next(c)) {
        xcom::Vertex * from = EDGE_from(e);
        xcom::Vertex * to = EDGE_to(e);
        fprintf(h, "\nedge: { sourcename:\"%u\" targetname:\"%u\" %s}",
                from->id(), to->id(),  "");
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
    count += m_map_prno2vpr_vec.count_mem();
    count += m_map_prno2stack.count_mem();
    count += sizeof(m_vp_count);
    count += m_vpr_vec.count_mem();
    count += m_max_version.count_mem();
    count += m_prno2ir.count_mem();
    count += sizeof(m_rg);
    return count;
}


//Clean version stack.
void PRSSAMgr::cleanPRNO2Stack()
{
    for (INT i = 0; i <= m_map_prno2stack.get_last_idx(); i++) {
        Stack<VPR*> * s = m_map_prno2stack.get(i);
        if (s != NULL) { delete s; }
    }
    m_map_prno2stack.clean();
}


//Dump ssa du stmt graph.
void PRSSAMgr::dumpSSAGraph(CHAR * name) const
{
    SSAGraph sa(m_rg, const_cast<PRSSAMgr*>(this));
    sa.dump(name, true);
}


CHAR * PRSSAMgr::dumpVP(IN VPR * v, OUT CHAR * buf) const
{
    sprintf(buf, "$%dV%d$%d", v->orgprno(), v->version(), v->newprno());
    return buf;
}


//This function dumps VPR structure and SSA DU info.
//have_renamed: set true if PRs have been renamed in construction.
void PRSSAMgr::dumpAllVPR() const
{
    if (g_tfile == NULL) { return; }
    note("\n==---- DUMP PRSSAMgr:VPR '%s' ----==\n", m_rg->getRegionName());

    VPRVec const* vpr_vec = getVPRVec();
    for (INT i = PRNO_UNDEF + 1; i <= vpr_vec->get_last_idx(); i++) {
        VPR * v = vpr_vec->get(i);
        ASSERT0(v != NULL);
        note("\nid%d:$%dv%d$%d: ", v->id(), v->orgprno(),
             v->version(), v->newprno());
        IR * def = SSA_def(v);
        if (v->version() != PRSSA_INIT_VERSION) {
            //After renaming, version is meaningless.
            //For convenient purpose, tolerate the pathological SSA form.
            //ASSERT0(def);
        }
        if (def != NULL) {
            ASSERT0(def->is_stmt());            
            if (def->isWritePR()) {
                prt("DEF:%s ($%d,id:%d)", IRNAME(def),
                    def->getPrno(), def->id());
            } else if (def->isCallStmt()) {
                prt("DEF:%s", IRNAME(def));
                if (def->hasReturnValue()) {
                    prt(" ($%d,id:%d)", def->getPrno(), def->id());
                } else {
                    prt(" NoRetVal??");
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
            IR * use = m_rg->getIR(i2);
            ASSERT0(use->is_pr());
            prt("($%d,id:%d)", use->getPrno(), IR_id(use));
            if (nexti >= 0) {
                prt(",");
            }
        }
    }
    fflush(g_tfile);
}


//Allocate VPR and ensure it is unique according to 'version' and 'prno'.
VPR * PRSSAMgr::allocVPR(UINT prno, UINT version)
{
    ASSERT0(prno != PRNO_UNDEF);    
    VPRVec * vec = m_map_prno2vpr_vec.get(prno);
    if (vec == NULL) {
        vec = new VPRVec();
        m_map_prno2vpr_vec.set(prno, vec);
    }

    VPR * v = vec->get(version);
    if (v != NULL) {
        return v;
    }

    ASSERTN(m_seg_mgr, ("SSA manager is not initialized"));
    v = allocVPR();
    v->initNoClean(m_seg_mgr);
    VPR_orgprno(v) = prno;
    VPR_newprno(v) = PRNO_UNDEF;
    VPR_version(v) = version;
    SSA_id(v) = m_vp_count++;
    SSA_def(v) = NULL;
    vec->set(version, v);
    m_vpr_vec.set(SSA_id(v), v);
    return v;
}


void PRSSAMgr::buildDUChain(IR * def, IR * use)
{
    ASSERT0(def->isWritePR() || def->isCallHasRetVal());
    ASSERT0(use->isReadPR());
    SSAInfo * ssainfo = def->getSSAInfo();
    if (ssainfo == NULL) {
        ssainfo = allocSSAInfo(def->getPrno());
        def->setSSAInfo(ssainfo);
        SSA_def(ssainfo) = def;
    }

    //You may be set multiple defs for use.
    ASSERTN(use->getSSAInfo() == NULL, ("use already has SSA info."));
    use->setSSAInfo(ssainfo);
    ssainfo->addUse(use);
}


//is_reinit: this function is invoked in reinit().
void PRSSAMgr::destroy(bool is_reinit)
{
    if (m_vp_pool == NULL) { return; }

    //Caution: if you do not destruct SSA prior to destory().
    //The reference to IR's SSA info will lead to undefined behaviors.
    //ASSERTN(!m_is_valid,
    //   ("Still in ssa mode, you should out of "
    //    "SSA before the destruction."));

    for (INT i = 0; i <= m_map_prno2vpr_vec.get_last_idx(); i++) {
        VPRVec * vpv = m_map_prno2vpr_vec.get((UINT)i);
        if (vpv != NULL) { delete vpv; }
    }

    cleanPRNO2Stack();

    for (INT i = 0; i <= m_vpr_vec.get_last_idx(); i++) {
        VPR * v = m_vpr_vec.get((UINT)i);
        if (v != NULL) {
            v->destroy();
        }
    }

    if (is_reinit) {
        m_map_prno2vpr_vec.clean();
        m_vpr_vec.clean();
        m_max_version.clean();
        m_prno2ir.clean();
    }

    removePhiFromBB();

    //Do not free irs in m_prno2ir.
    smpoolDelete(m_vp_pool);
    m_vp_pool = NULL;
}


bool PRSSAMgr::dump() const
{
    if (g_tfile == NULL) { return false; }
    note("\n==---- DUMP %s '%s' ----==", getPassName(), m_rg->getRegionName());
    dumpAllVPR();

    BBList * bbl = m_rg->getBBList();
    List<IR const*> lst;
    List<IR const*> opnd_lst;
    INT orgindent = g_indent;
    for (IRBB * bb = bbl->get_head();
         bb != NULL; bb = bbl->get_next()) {
        note("\n--- BB%d ---", bb->id());
        for (IR * ir = BB_first_ir(bb);
             ir != NULL; ir = BB_next_ir(bb)) {
            g_indent += 4;
            note("\n------------------");
            dumpIR(ir, m_rg);
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
                VPR * vp = (VPR*)res->getSSAInfo();
                ASSERT0(vp);
                prt("$%dv%d$%d ", vp->orgprno(), vp->version(), vp->newprno());
            } else {
                prt("--");
            }
            prt(" <= ");
            if (opnd_lst.get_elem_count() != 0) {
                UINT i = 0, n = opnd_lst.get_elem_count() - 1;
                for (IR const* opnd = opnd_lst.get_head(); opnd != NULL;
                     opnd = opnd_lst.get_next(), i++) {
                    VPR * vp = (VPR*)PR_ssainfo(opnd);
                    ASSERT0(vp);
                    prt("$%dv%d$%d", vp->orgprno(),
                        vp->version(), vp->newprno());
                    if (i < n) { prt(","); }
                }
            } else {
                prt("--");
            }
            g_indent -= 4;            
        }
    }
    g_indent = orgindent;
    fflush(g_tfile);
    return true;
}


//Initialize VPR for each PR.
IR * PRSSAMgr::initVP(IN IR * ir)
{
    IR * prres = ir->getResultPR();

    //Process result.
    if (prres != NULL) {
        UINT prno = prres->getPrno();
        ir->setSSAInfo(allocVPR(prno, PRSSA_INIT_VERSION));
        if (m_prno2ir.get(prno) == NULL) {
            m_prno2ir.set(prno, ir);
        }
    }

    //Process opnd.
    m_iter.clean();
    for (IR * kid = iterInit(ir, m_iter);
         kid != NULL; kid = iterNext(m_iter)) {
        if (ir->is_rhs(kid) && kid->is_pr()) {
            PR_ssainfo(kid) = allocVPR(PR_no(kid), PRSSA_INIT_VERSION);
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
        //Generate VPR for non-phi stmt.
        IR * res = initVP(ir);
        for (IR * r = res; r != NULL; r = r->get_next()) {
            mustdef_pr.bunion(r->getPrno());
        }
    }
}


void PRSSAMgr::insertPhi(UINT prno, IN IRBB * bb)
{
    UINT num_opnd = m_cfg->getInDegree(m_cfg->getVertex(bb->id()));
    IR * pr = m_prno2ir.get(prno);
    ASSERT0(pr);

    //Here each operand and result of phi set to same type.
    //They will be revised to correct type during renaming.
    IR * phi = m_rg->buildPhi(pr->getPrno(), pr->getType(), num_opnd);

    m_rg->allocRefForPR(phi);

    for (IR * opnd = PHI_opnd_list(phi);
         opnd != NULL; opnd = opnd->get_next()) {
        opnd->copyRef(phi, m_rg);
    }

    BB_irlist(bb).append_head(phi);

    initVP(phi);
}


//Find livein def-stmt through given 'start' BB.
//start: the BB that begin to do searching.
static bool findLiveInDef(IRCFG * cfg,
                          VPRVec const* vprvec,
                          IRBB const* start,
                          VPR ** livein_def,
                          VPR ** init_version)
{
    ASSERT0(livein_def && init_version && start);
    IRBB const* meetup = cfg->getEntry();
    ASSERT0(meetup);
    //Record the initial version.
    *init_version = vprvec->findInitVersion();

    xcom::List<IRBB const*> wl;
    cfg->get_preds(wl, start);
    xcom::TTab<UINT> visited;
    while (wl.get_elem_count() != 0) {
        IRBB const* t = wl.remove_head();
        if (t == meetup) { continue; }

        VPR * vpr = vprvec->findVPR(t->id());
        if (vpr != NULL) {
            *livein_def = vpr;
            return true;
        }

        visited.append(t->id());
        for (xcom::EdgeC * el = cfg->getVertex(t->id())->getInList();
             el != NULL; el = el->get_next()) {
            UINT pred = (UINT)el->getFromId();
            if (!visited.find(pred)) {
                wl.append_tail(cfg->getBB(pred));
            }
        }
    }
    return false;
}


//Find livein def-stmt through given 'start' BB.
//This function will iterate dom tree bottom up from 'start' until the
//anticipated BB found.
//start: the BB that begin to do searching.
static void findLiveInDefInDomSet(IRCFG * cfg,
                                  VPRVec const* vprvec,
                                  IRBB const* start,
                                  VPR ** livein_def,
                                  VPR ** init_version)
{
    ASSERT0(livein_def && init_version && start);
    //Record the initial version.
    *init_version = vprvec->findInitVersion();

    for (UINT bbid = start->id(); bbid != BBID_UNDEF;
         bbid = ((DGraph*)cfg)->get_idom(bbid)) {         
        VPR * vpr = vprvec->findVPR(bbid);
        if (vpr != NULL) {
            *livein_def = vpr;
            return;
        }
    }
}


//Insert operand at given position.
//pos: position of operand, start at 0.
//     Each operand correspond to in-edge on CFG.
IR * PRSSAMgr::insertOpndAt(IR * phi, UINT pos, IRBB const* pred)
{
    Region * rg = m_rg;
    UINT i = 0;
    IR * marker = PHI_opnd_list(phi);
    IR * last = NULL;
    for (; marker != NULL && i <= pos; marker = marker->get_next(), i++) {
        last = marker;
    }
    SSAInfo const* info = phi->getSSAInfo();
    ASSERT0(info);

    //Get original PRNO before SSA construction.
    UINT org_prno = ((VPR*)info)->orgprno();
    ASSERT0(org_prno != PRNO_UNDEF);
    VPRVec const* vprvec = getVPRVecByPRNO(org_prno);
    ASSERT0(vprvec);
    IRCFG * cfg = rg->getCFG();

    //Find the latest live-in DEF SSAInfo of operand.
    VPR * livein_def = NULL;
    VPR * init_version = NULL;
    //findLiveInDefInDomSet(cfg, vprvec, pred, &livein_def, &init_version);
    findLiveInDef(cfg, vprvec, pred, &livein_def, &init_version);

    //Add current PR into USE set of lived-in VPR.
    ASSERT0(livein_def);
    IR * def = livein_def->getDef();
    ASSERT0(def);    
    IR * newopnd = m_rg->buildPRdedicated(def->getResultPR()->getPrno(),
        def->getType());
    newopnd->setSSAInfo(livein_def);

    //Add USE to SSAInfo.
    livein_def->addUse(newopnd);
    if (marker != NULL) {
        //Insert operand into list.
        ((CPhi*)phi)->insertOpndBefore(marker, newopnd);
        return newopnd;
    }

    //Append a new operand to list.
    ASSERT0(pos >= 0 && i == pos);
    //'last' may be NULL, because the operand list may be empty before
    //insertion. During several CFG edge removing and building,
    //there may appear single operand PHI.
    //If CFG optimization perform removing single edge then
    //adding a new edge, the PHI operand is empty when adding the new edge.
    //e.g:Before adding a new edge.
    //  BB13:
    //  Phi: MD13V4 <-| UsedBy :
    xcom::add_next(&PHI_opnd_list(phi), &last, newopnd);
    return newopnd;
}


//After adding BB or change BB successor,
//you need add the related PHI operand if BB successor has PHI stmt.
void PRSSAMgr::addSuccessorDesignatePhiOpnd(IRBB * bb, IRBB * succ)
{
    UINT pos = m_cfg->WhichPred(bb, succ);
    IRListIter it;
    for (IR * ir = BB_irlist(succ).get_head(&it);
         ir != NULL; ir = BB_irlist(succ).get_next(&it)) {
        if (!ir->is_phi()) { break; }
        insertOpndAt(ir, pos, bb);
        ASSERT0(xcom::cnt_list(PHI_opnd_list(ir)) == succ->getNumOfPred(m_cfg));
        //addMDSSAOcc(opnd);
    }
}


//Insert phi for PR.
//defbbs: record BBs which defined the PR identified by 'prno'.
void PRSSAMgr::placePhiForPR(UINT prno,
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
        //visited.bunion(defbb->id());
    }

    while (wl.get_elem_count() != 0) {
        IRBB * bb = wl.remove_head();

        //Each BB in Set dfcs is in dominance frontier of 'bb'.
        xcom::BitSet const* dfcs = dfm.getDFControlSet(bb->id());
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

    VPR * vp = (VPR*)PHI_ssainfo(phi);
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
    BBList * bblst = m_rg->getBBList();
    //All objects allocated and recorded in pr2defbb are used for local purpose,
    //and will be destoied before leaving this function.
    Vector<List<IRBB*>*> pr2defbb(bblst->get_elem_count());

    for (IRBB * bb = bblst->get_head(); bb != NULL; bb = bblst->get_next()) {
        DefSBitSet * bs = bs_mgr.allocSBitSet();
        defined_prs_vec.set(bb->id(), bs);
        collectDefinedPR(bb, *bs);

        //Regard all defined PR as effect, and they will be versioned later.
        effect_prs.bunion(*bs);

        //Record which BB defined these effect prs.
        DefSBitSetIter cur = NULL;
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
    DefSBitSetIter cur = NULL;
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


//Rename opnd, except PHI.
//Walk through RHS expression of 'ir' to rename PR's VPR.
//bb: the BB that ir belongs to.
void PRSSAMgr::renameRHS(IR * ir, IRBB * bb)
{
    ASSERT0(!ir->is_phi());
    m_iter.clean();
    for (IR * opnd = iterInit(ir, m_iter);
         opnd != NULL; opnd = iterNext(m_iter)) {
        if (!ir->is_rhs(opnd) || !opnd->is_pr()) {
            continue;
        }
    
        //Get the top-version on stack.
        Stack<VPR*> * vs = mapPRNO2VPStack(PR_no(opnd));
        ASSERT0(vs);
        VPR * topv = vs->get_top();
        if (topv == NULL) {
            //prno has no top-version, it has no def, may be parameter.
            ASSERT0(PR_ssainfo(opnd));
            ASSERTN(VPR_version((VPR*)PR_ssainfo(opnd)) == 0,
                    ("parameter only has first version"));
            continue;
        }
    
        //e.g: pr1 = pr2(vp1)
        //    vp1 will be renamed to vp2, so vp1 does not
        //    occur in current IR any more.
        VPR * curv = (VPR*)PR_ssainfo(opnd);
        ASSERT0(curv && curv->orgprno() == PR_no(opnd));
    
        //Let latest version VPR regard current opnd as an USE.
        if (VPR_version(topv) == 0) {
            //Each opnd only meet once.
            ASSERT0(curv == topv);
            ASSERT0(!topv->findUse(opnd));
            topv->addUse(opnd);
            continue;
        }

        if (curv != topv) {
            //curv may be ver0.
            //Current ir does not refer the old version VPR any more.
            ASSERT0(VPR_version(curv) == 0 || curv->findUse(opnd));
            ASSERT0(VPR_version(curv) == 0 || SSA_def(curv) != NULL);
            ASSERT0(!topv->findUse(opnd));
    
            curv->removeUse(opnd);
            PR_ssainfo(opnd) = topv;
            topv->addUse(opnd);
        }
    }
}


//Remove PR-SSA Use-Def chain.
//e.g:ir=...
//    ...=ir //S1
//If S1 deleted, ir should be removed from its UseSet in SSAInfo.
//NOTE: If ir is an IR tree, e.g: add(pr1, pr2), removing 'add' means
//pr1 and pr2 will be removed as well. Therefore pr1 pr2's SSAInfo will be
//updated as well.
void PRSSAMgr::removePRSSAUse(IR * ir)
{
    if (ir->is_stmt()) {
        if (ir->isWritePR() || ir->isCallStmt()) {
            SSAInfo * prssainfo = ir->getSSAInfo();
            if (prssainfo != NULL) {
                prssainfo->cleanDU();
            }
        }
    } else {
        SSAInfo * prssainfo = ir->getSSAInfo();
        if (prssainfo != NULL) {
            prssainfo->removeUse(ir);
        }
    }
    //Whole IR tree may be removed via this function recursively.
    //Maintain the SSAInfo of read-pr/write-pr operation.
    for (UINT i = 0; i < IR_MAX_KID_NUM(ir); i++) {
        for (IR * x = ir->getKid(i); x != NULL; x = x->get_next()) {
            removePRSSAUse(x);
        }
    }
}


//Check each USE of stmt, remove the expired one which is not reference
//the memory any more that stmt defined.
bool PRSSAMgr::removeExpiredDUForStmt(IR * stmt, Region * rg)
{
    ASSERT0(stmt->is_stmt());
    SSAInfo * ssainfo = stmt->getSSAInfo();
    if (ssainfo == NULL) { return false; }

    SSAUseIter si;
    UINT prno = 0;
    if (SSA_def(ssainfo) != NULL) {
        prno = SSA_def(ssainfo)->getPrno();
    } else {
        //Without DEF.
        return false;
    }

    //Check if use referenced the number of PR which defined by SSA_def.
    INT ni = -1;
    bool change = false;
    for (INT i = SSA_uses(ssainfo).get_first(&si); si != NULL; i = ni) {
        ni = SSA_uses(ssainfo).get_next(i, &si);
        IR const* use = rg->getIR(i);
        if (use->is_pr() && PR_no(use) == prno) { continue; }
    
        ssainfo->removeUse(use);
        change = true;
    }
    return change;
}


//Rename vp from current version to the top-version on stack if it exist.
void PRSSAMgr::renameBB(IN IRBB * bb)
{
     for (IR * ir = BB_first_ir(bb); ir != NULL; ir = BB_next_ir(bb)) {
        if (!ir->is_phi()) {
            renameRHS(ir, bb);
        }
        IR * res = ir->getResultPR();
        if (res == NULL) { continue; } 

        //Rename result, include phi.
        ASSERT0(res->is_single());
        VPR * resvp = (VPR*)res->getSSAInfo();
        ASSERT0(resvp);
        ASSERTN(resvp->orgprno() == res->getPrno(),
                ("PRNO is unchanged until stripping version"));
        UINT orgprno = resvp->orgprno();
        UINT maxv = m_max_version.get(orgprno);
        VPR * new_v = allocVPR(orgprno, maxv + 1);
        m_max_version.set(orgprno, maxv + 1);

        mapPRNO2VPStack(orgprno)->push(new_v);
        res->setSSAInfo(new_v);
        SSA_def(new_v) = ir;
    }
}


Stack<VPR*> * PRSSAMgr::mapPRNO2VPStack(UINT prno)
{
    Stack<VPR*> * stack = m_map_prno2stack.get(prno);
    if (stack == NULL) {
        stack = new Stack<VPR*>();
        m_map_prno2stack.set(prno, stack);
    }
    return stack;
}


//The allocated object will be destroied at destoryPRNO2VP().
static PRNO2VP * allocPRNO2VP(UINT bbid, BB2VPMap & bb2vp)
{
    PRNO2VP * prno2vp = new PRNO2VP();
    bb2vp.set(bbid, prno2vp);
    return prno2vp;
}


static void destoryPRNO2VP(UINT bbid, PRNO2VP * prno2vp, BB2VPMap & bb2vpmap)
{
    bb2vpmap.set(bbid, NULL);
    delete prno2vp;
}


void PRSSAMgr::handleBBRename(IRBB * bb,
                              DefSBitSet const& defined_prs,
                              IN OUT BB2VPMap & bb2vp)
{
    ASSERT0(bb2vp.get(bb->id()) == NULL);
    PRNO2VP * prno2vp = allocPRNO2VP(bb->id(), bb2vp);
    DefSBitSetIter cur = NULL;
    for (INT prno = defined_prs.get_first(&cur);
         prno >= 0; prno = defined_prs.get_next(prno, &cur)) {
        VPR * vp = mapPRNO2VPStack(prno)->get_top();
        ASSERT0(vp);
        prno2vp->set(vp->orgprno(), vp);
    }
    renameBB(bb);

    //Rename PHI operand in which successor BB placed.
    List<IRBB*> succs;
    m_cfg->get_succs(succs, bb);
    if (succs.get_elem_count() <= 0) { return; }

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
        IRListIter ct;
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
            VPR * old = (VPR*)PR_ssainfo(opnd);
            ASSERT0(old != NULL);
            VPR * topv = mapPRNO2VPStack(old->orgprno())->get_top();
            ASSERT0(topv != NULL);

            old->removeUse(opnd);

            if (SSA_def(topv) != NULL) {
                //topv might be zero version.
                IR * defres = SSA_def(topv)->getResultPR(topv->orgprno());
                ASSERT0(defres);

                IR * new_opnd = m_rg->buildPRdedicated(
                    defres->getPrno(), defres->getType());
                new_opnd->copyRef(defres, m_rg);
                xcom::replace(&PHI_opnd_list(ir), opnd, new_opnd);
                IR_parent(new_opnd) = ir;
                m_rg->freeIRTree(opnd);
                opnd = new_opnd;

                //Phi should have same type with opnd.
                IR_dt(ir) = IR_dt(defres);
            }

            PR_ssainfo(opnd) = topv;

            //Add version0 opnd here, means opnd will be add to ver0
            //use-list if topv is version0. So one does not need to
            //add version0 at placePhi.
            topv->addUse(opnd);
        }
    }
}


//Linear renaming algorithm.
//defined_prs_vec: for each BB, indicate PRs which has been defined.
void PRSSAMgr::renameInDomTreeOrder(IRBB * root,
                                    xcom::Graph const& domtree,
                                    Vector<DefSBitSet*> const& defined_prs_vec)
{
    Stack<IRBB*> stk;
    UINT n = m_rg->getBBList()->get_elem_count();
    xcom::BitSet visited(n / BIT_PER_BYTE);
    BB2VPMap bb2vpmap(n);
    IRBB * v;
    stk.push(root);
    List<IR*> lst; //for tmp use.
    while ((v = stk.get_top()) != NULL) {
        if (!visited.is_contain(v->id())) {
            visited.bunion(v->id());
            DefSBitSet * defined_prs = defined_prs_vec.get(v->id());
            ASSERT0(defined_prs);
            handleBBRename(v, *defined_prs, bb2vpmap);
        }

        xcom::Vertex const* bbv = domtree.getVertex(v->id());
        xcom::EdgeC const* c = VERTEX_out_list(bbv);
        bool all_visited = true;
        while (c != NULL) {
            xcom::Vertex * dom_succ = c->getTo();
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
            PRNO2VP * prno2vp = bb2vpmap.get(v->id());
            ASSERT0(prno2vp);
            DefSBitSet const* defined_prs = defined_prs_vec.get(v->id());
            ASSERT0(defined_prs);

            DefSBitSetIter cur = NULL;
            for (INT i = defined_prs->get_first(&cur);
                 i >= 0; i = defined_prs->get_next(i, &cur)) {
                Stack<VPR*> * vs = mapPRNO2VPStack(i);
                ASSERT0(vs->get_bottom() != NULL);
                VPR * vp = prno2vp->get(vs->get_top()->orgprno());
                while (vs->get_top() != vp) {
                    vs->pop();
                }
            }

            //vpmap is useless from now on.
            destoryPRNO2VP(v->id(), prno2vp, bb2vpmap);
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
void PRSSAMgr::rename(DefSBitSet const& effect_prs,
                      Vector<DefSBitSet*> const& defined_prs_vec,
                      xcom::Graph const& domtree)
{
    START_TIMER(t, "PRSSA: Rename");
    if (m_rg->getBBList()->get_elem_count() == 0) { return; }
    DefSBitSetIter cur = NULL;
    for (INT prno = effect_prs.get_first(&cur);
         prno >= 0; prno = effect_prs.get_next(prno, &cur)) {
        VPR * vp = allocVPR(prno, PRSSA_INIT_VERSION);
        mapPRNO2VPStack(prno)->push(vp);
    }

    ASSERT0(m_cfg->getEntry());
    renameInDomTreeOrder(m_cfg->getEntry(), domtree, defined_prs_vec);
    END_TIMER(t, "PRSSA: Rename");
}


void PRSSAMgr::destructBBSSAInfo(IRBB * bb)
{
    IRListIter ct;
    IRListIter next_ct;
    BB_irlist(bb).get_head(&next_ct);
    ct = next_ct;
    for (; ct != BB_irlist(bb).end(); ct = next_ct) {
        next_ct = BB_irlist(bb).get_next(next_ct);
        IR * ir = ct->val();
        if (!ir->is_phi()) { break; }

        stripPhi(ir, ct);
        BB_irlist(bb).remove(ct);
        m_rg->freeIRTree(ir);
    }
}


void PRSSAMgr::destructionInDomTreeOrder(IRBB * root, xcom::Graph & domtree)
{
    Stack<IRBB*> stk;
    UINT n = m_rg->getBBList()->get_elem_count();
    xcom::BitSet visited(n / BIT_PER_BYTE);
    BB2VPMap bb2vp(n);
    IRBB * v;
    stk.push(root);
    while ((v = stk.get_top()) != NULL) {
        if (!visited.is_contain(v->id())) {
            visited.bunion(v->id());
            destructBBSSAInfo(v);
        }

        xcom::Vertex * bbv = domtree.getVertex(v->id());
        ASSERTN(bbv, ("dom tree is invalid."));

        xcom::EdgeC * c = VERTEX_out_list(bbv);
        bool all_visited = true;
        while (c != NULL) {
            xcom::Vertex * dom_succ = c->getTo();
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

    BBList * bblst = m_rg->getBBList();
    if (bblst->get_elem_count() == 0) { return; }
    ASSERT0(m_cfg->getEntry());
    destructionInDomTreeOrder(m_cfg->getEntry(), domtree);
    cleanPRSSAInfo();
    m_is_valid = false;

    END_TIMER(t, "PRSSA: destruction in dom tree order");
}


//Return true if inserting copy at the head of fallthrough BB
//of current BB's predessor.
//Note that do not free phi at this function, it will be freed
//by user.
void PRSSAMgr::stripPhi(IR * phi, IRListIter phict)
{
    IRBB * bb = phi->getBB();
    ASSERT0(bb);

    xcom::Vertex const* vex = m_cfg->getVertex(bb->id());
    ASSERT0(vex);

    //Temprarory RP to hold the result of PHI.
    IR * phicopy = m_rg->buildPR(phi->getType());
    phicopy->setRefMD(m_rg->genMDforPR(PR_no(phicopy),
        phicopy->getType()), m_rg);
    phicopy->cleanRefMDSet();
    IR * opnd = PHI_opnd_list(phi);

    //opnd may be CONST, LDA, PR.
    //ASSERT0(opnd->is_pr());
    ASSERT0(PHI_ssainfo(phi));

    UINT pos = 0;
    for (xcom::EdgeC * el = vex->getInList(), * nextel = NULL;
         el != NULL; el = nextel, opnd = opnd->get_next(), pos++) {
        ASSERT0(find_position(vex->getInList(), el) == pos);
        nextel = el->get_next();
        INT pred = el->getFromId();

        ASSERT0(opnd && opnd->is_exp());
        IR * opndcopy = m_rg->dupIRTree(opnd);
        if (!opndcopy->is_const()) {
            opndcopy->copyRefForTree(opnd, m_rg);
        }

        //The copy will be inserted into related predecessor.
        IR * store_to_phicopy = m_rg->buildStorePR(PR_no(phicopy),
            phicopy->getType(), opndcopy);
        store_to_phicopy->copyRef(phicopy, m_rg);

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
                    //Insert basic block to hold the copy.
                    IRBB * newbb = m_rg->allocBB();
                    m_rg->getBBList()->insert_after(newbb, p);
                    m_cfg->addBB(newbb);
                    m_cfg->insertVertexBetween(p->id(), fallthrough->id(),
                                               newbb->id());
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
            PR_ssainfo(opnd)->removeUse(opnd);
        }
    }

    IR * substitue_phi = m_rg->buildStorePR(PHI_prno(phi),
        phi->getType(), phicopy);
    substitue_phi->copyRef(phi, m_rg);
    BB_irlist(bb).insert_before(substitue_phi, phict);
    PHI_ssainfo(phi) = NULL;
}


void PRSSAMgr::removePhiFromBB()
{
    BBList * bblst = m_rg->getBBList();
    BBListIter bbit = NULL;
    for (bblst->get_head(&bbit); bbit != NULL; bbit = bblst->get_next(bbit)) {
        IRBB * bb = bbit->val();
        IRListIter irit = NULL;
        IRListIter next_irit = NULL;
        for (BB_irlist(bb).get_head(&irit); irit != NULL; irit = next_irit) {
            next_irit = BB_irlist(bb).get_next(irit);
            IR * ir = irit->val();
            if (!ir->is_phi()) {
                //There is no phi any more.
                break;
            }
            PHI_ssainfo(ir) = NULL;
            BB_irlist(bb).remove(irit);
            m_rg->freeIRTree(ir);
        }
    }
}


//This function verify def/use information of PHI stmt.
//If vpinfo is available, the function also check VPR_prno of phi operands.
//is_vpinfo_avail: set true if VPR information is available.
//before_strip_version: true if this function invoked before striping version.
bool PRSSAMgr::verifyPhi(bool is_vpinfo_avail, bool before_strip_version)
{
    DUMMYUSE(is_vpinfo_avail);
    BBList * bblst = m_rg->getBBList();
    List<IRBB*> preds;
    for (IRBB * bb = bblst->get_head(); bb != NULL; bb = bblst->get_next()) {
        m_cfg->get_preds(preds, bb);
        IRListIter ct;
        for (BB_irlist(bb).get_head(&ct); ct != BB_irlist(bb).end();
             ct = BB_irlist(bb).get_next(ct)) {
            IR const* ir = ct->val();
            if (!ir->is_phi()) { continue; }

            //Check phi result.
            VPR * resvp = (VPR*)PHI_ssainfo(ir);
            if (is_vpinfo_avail) {
                UINT prno = before_strip_version ?
                    resvp->orgprno() : resvp->newprno();
                ASSERTN(prno == PHI_prno(ir), ("prno of VPR is unmatched"));
            }           

            //Check the number of phi opnds.
            UINT num_opnd = 0;

            for (IR const* opnd = PHI_opnd_list(ir);
                 opnd != NULL; opnd = opnd->get_next()) {
                //Opnd may be PR, CONST or LDA.
                if (is_vpinfo_avail) {
                    UINT prno = before_strip_version ?
                        VPR_orgprno(PR_ssainfo(opnd)) :
                        VPR_newprno(PR_ssainfo(opnd));
                    ASSERTN(prno == PR_no(opnd),
                            ("prno of VPR is unmatched"));
                }
                
                //Ver0 is input parameter, and it has no SSA_def.
                //ASSERT0(VPR_version(PR_ssainfo(opnd)) > 0);

                num_opnd++;
            }

            ASSERTN(num_opnd == preds.get_elem_count(),
                   ("The number of phi operand must same with "
                    "the number of BB predecessors."));

            //Check SSA uses.
            SSAUseIter vit = NULL;
            for (INT i = SSA_uses(resvp).get_first(&vit);
                 vit != NULL; i = SSA_uses(resvp).get_next(i, &vit)) {
                IR * use = m_rg->getIR(i);
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


//Check the consistency for IR_PR if VPR_newprno == PR_no.
//This function only can be invoked immediately
//after rename() and before refinePhi(), because refinePhi() might
//clobber VPR information, that leads VPR_orgprno() to be invalid.
bool PRSSAMgr::verifyPRNOofVP()
{
    ConstIRIter ii;
    BBList * bblst = m_rg->getBBList();
    BBListIter ct;
    for (IRBB * bb = bblst->get_head(&ct);
         bb != NULL; bb = bblst->get_next(&ct)) {
         for (IR * ir = BB_first_ir(bb); ir != NULL; ir = BB_next_ir(bb)) {
            ii.clean();
            for (IR const* opnd = iterInitC(ir, ii);
                 opnd != NULL; opnd = iterNextC(ii)) {
                if (opnd->is_pr()) {
                    ASSERT0(PR_no(opnd) == VPR_orgprno(PR_ssainfo(opnd)));
                }
            }
         }
     }
    return true;
}


//Verify VPR after striping version.
bool PRSSAMgr::verifyVPR()
{
    //Check version for each vp.
    xcom::BitSet defset;
    for (INT i = 1; i <= m_vpr_vec.get_last_idx(); i++) {
        VPR * v = m_vpr_vec.get(i);
        ASSERT0(v != NULL);
        IR const* def = v->getDef();
        if (def == NULL) {
            //ver0 used to indicate the Region live-in PR.
            //It may be a parameter.
            ASSERTN(v->version() == PRSSA_INIT_VERSION,
                    ("Nondef vp's version must be initial version, "
                     "initial version means there is no DEF"));
        } else {
            ASSERT0(def->getSSAInfo() == v);
            ASSERTN(v->version() != PRSSA_INIT_VERSION,
                    ("version can not be initial version, "
                     "initial version means there is no DEF"));
            ASSERT0(def->is_stmt());

            ASSERTN(!defset.is_contain(def->id()),
                    ("DEF for each pr+version must be unique."));
            defset.bunion(def->id());
        }

        IR const* respr = NULL;
        UINT defprno = 0;
        if (def != NULL) {
            respr = const_cast<IR*>(def)->getResultPR(v->newprno());
            ASSERTN(respr, ("Expect stmt result PR to be PR%d", v->newprno()));

            defprno = respr->getPrno();
            ASSERTN(defprno != PRNO_UNDEF, ("invalid PR built"));
        }

        SSAUseIter vit = NULL;
        UINT opndprno = PRNO_UNDEF;
        for (INT i2 = SSA_uses(v).get_first(&vit);
             vit != NULL; i2 = SSA_uses(v).get_next(i2, &vit)) {
            IR * use = m_rg->getIR(i2);
            ASSERT0(use->is_pr() || use->is_const());
            if (!use->is_pr()) { continue; }

            if (opndprno == PRNO_UNDEF) {
                opndprno = PR_no(use);
            } else {
                //All USE should have same PR no.
                ASSERT0(opndprno == PR_no(use));
            }

            //Each USE of current VPR must be defined by same stmt.
            ASSERT0(use->getSSAInfo() == v);            
        }

        if (opndprno != PRNO_UNDEF && defprno != PRNO_UNDEF) {
            //DEF should have same PRNO with USEs.
            ASSERT0(opndprno == defprno);
        }
    }
    return true;
}


static void verify_ssainfo_helper(IR * ir, xcom::BitSet & defset, Region * rg)
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
    UINT defprno = PRNO_UNDEF;
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
            if (opndprno == PRNO_UNDEF) {
                opndprno = PR_no(use);
            } else {
                //All opnd should have same PR no.
                ASSERT0(opndprno == PR_no(use));
            }

            //Each USE of current SSAInfo must be defined by same stmt.
            ASSERT0(PR_ssainfo(use) == ssainfo);
        }
    }

    if (opndprno != PRNO_UNDEF && defprno != PRNO_UNDEF) {
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
    BBList * bbl = m_rg->getBBList();
    BBListIter ct;
    for (bbl->get_head(&ct); ct != bbl->end(); ct = bbl->get_next(ct)) {
        IRBB * bb = ct->val();
        IRListIter ctir;
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
                    verify_ssainfo_helper(x, defset, m_rg);
                }
            }
        }
    }
    return true;
}


//This function perform SSA destruction via scanning BB in sequential order.
void PRSSAMgr::destruction(OptCtx * oc)
{
    BBList * bblst = m_rg->getBBList();
    if (bblst->get_elem_count() == 0) { return; }
    UINT bbcnt = bblst->get_elem_count();
    BBListIter bbct;
    for (bblst->get_head(&bbct);
         bbct != bblst->end(); bbct = bblst->get_next(bbct)) {
        IRBB * bb = bbct->val();
        ASSERT0(bb);
        destructBBSSAInfo(bb);
    }
    if (bblst->get_elem_count() != bbcnt) {
        ASSERT0(oc);
        oc->set_flag_if_cfg_changed();
        //Each pass maintain CFG by default.
        OC_is_cfg_valid(*oc) = true;
    }

    //Clean SSA info to avoid unnecessary abort or assert.
    cleanPRSSAInfo();
    m_is_valid = false;
}


//Set SSAInfo of IR to be NULL to inform optimizer that IR is not in SSA form.
void PRSSAMgr::cleanPRSSAInfo()
{
    BBList * bblst = m_rg->getBBList();
    BBListIter bbct = NULL;
    for (bblst->get_head(&bbct);
         bbct != bblst->end(); bbct = bblst->get_next(bbct)) {
        IRBB * bb = bbct->val();
        ASSERT0(bb);

        IRListIter irct = NULL;
        for (BB_irlist(bb).get_head(&irct);
             irct != BB_irlist(bb).end(); irct = BB_irlist(bb).get_next(irct)) {
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
//Return true if there is phi removed.
bool PRSSAMgr::refinePhi()
{
    List<IRBB*> wl;
    return refinePhi(wl);
}


//This function revise phi data type, and remove redundant phi.
//wl: work list for temporary used.
//Return true if there is phi removed.
bool PRSSAMgr::refinePhi(List<IRBB*> & wl)
{
    START_TIMER(t, "PRSSA: Refine phi");

    BBList * bblst = m_rg->getBBList();
    BBListIter ct = NULL;

    wl.clean();
    //Estimate memory usage.
    BitSet in_list(bblst->get_elem_count() / BITS_PER_BYTE / 4);
    for (bblst->get_head(&ct); ct != bblst->end(); ct = bblst->get_next(ct)) {
        IRBB * bb = ct->val();
        ASSERT0(bb);
        IR const* first = BB_first_ir(bb);
        if (first != NULL && first->is_phi()) {
            wl.append_tail(bb);
            in_list.bunion(bb->id());
        }
    }

    bool remove = false;
    IRBB * bb = NULL;
    while ((bb = wl.remove_head()) != NULL) {
        in_list.diff(bb->id());
        IRListIter irct = NULL;
        IRListIter nextirct = NULL;
        for (BB_irlist(bb).get_head(&nextirct), irct = nextirct;
             irct != BB_irlist(bb).end(); irct = nextirct) {
            nextirct = BB_irlist(bb).get_next(nextirct);
            IR * ir = irct->val();
            if (!ir->is_phi()) { break; }
            
            IR * common_def = NULL;
            if (!isRedundantPHI(ir, &common_def)) {
                revisePhiDataType(ir, m_rg);
                continue;
            }

            //PHI is redundant, revise SSAInfo before remove the PHI.
            for (IR const* opnd = PHI_opnd_list(ir);
                 opnd != NULL; opnd = opnd->get_next()) {
                ASSERT0(opnd->is_phi_opnd());
                if (!opnd->is_pr()) { continue; }

                SSAInfo * si = PR_ssainfo(opnd);
                ASSERTN(si, ("Miss SSAInfo."));

                si->removeUse(opnd);

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

                //TO BE CONFIRMED:Why did you say the result PR of common-def
                //have to be same with PHI?
                //ASSERT0(common_def->getResultPR(PHI_prno(ir)));
                //ASSERTN(common_def->getResultPR(PHI_prno(ir))->getPrno()
                //        == PHI_prno(ir), ("not same PR"));

                IR * respr = common_def->getResultPR();
                ASSERT0(respr);

                SSAInfo * commdef_ssainfo = respr->getSSAInfo();
                ASSERT0(commdef_ssainfo);

                SSA_uses(commdef_ssainfo).bunion(SSA_uses(curphi_ssainfo));

                SSAUseIter si = NULL;
                for (INT i = SSA_uses(curphi_ssainfo).get_first(&si);
                     si != NULL;
                     i = SSA_uses(curphi_ssainfo).get_next(i, &si)) {
                    IR * use = m_rg->getIR(i);
                    ASSERT0(use->is_pr());

                    ASSERT0(PR_ssainfo(use) &&
                            PR_ssainfo(use) == curphi_ssainfo);

                    PR_ssainfo(use) = commdef_ssainfo;
                    PR_no(use) = respr->getPrno();
                }
            }

            ((VPR*)curphi_ssainfo)->cleanMember();
            curphi_ssainfo->cleanDU();
            BB_irlist(bb).remove(irct);
            m_rg->freeIR(ir);
            remove = true;
        }
    }
    END_TIMER(t, "PRSSA: Refine phi");
    return remove;
}


//Before removing BB or change BB successor,
//you need remove the related PHI operand if BB successor has PHI.
void PRSSAMgr::removeSuccessorDesignatePhiOpnd(IRBB * bb, IRBB * succ)
{
    ASSERT0(bb && succ);
    UINT pos = m_cfg->WhichPred(bb, succ);
    for (IR * ir = BB_first_ir(succ); ir != NULL; ir = BB_next_ir(succ)) {
        if (!ir->is_phi()) { break; }

        ASSERT0(xcom::cnt_list(PHI_opnd_list(ir)) == succ->getNumOfPred(m_cfg));

        IR * opnd;
        UINT lpos = pos;
        for (opnd = PHI_opnd_list(ir); lpos != 0; opnd = opnd->get_next()) {
            ASSERT0(opnd);
            lpos--;
        }

        if (opnd == NULL) {
            //PHI does not contain any operand.
            continue;
        }

        opnd->removeSSAUse();
        ((CPhi*)ir)->removeOpnd(opnd);
        m_rg->freeIRTree(opnd);
    }
}


//This function revise phi data type, and remove redundant phi.
void PRSSAMgr::stripVersionForBBList()
{
    START_TIMER(t, "PRSSA: Strip PR version");

    BBList * bblst = m_rg->getBBList();
    if (bblst->get_elem_count() == 0) { return; }

    BBListIter ct = NULL;
    xcom::BitSet visited;

    //Ensure the first allocation of bitset could
    //accommodata the last vp id.
    visited.alloc(m_vp_count / BITS_PER_BYTE + 1);

    //Why not strip all VPR at once by iterating m_vpr_vec?
    //Just like:
    //  for (INT i = 1; i <= m_vpr_vec.get_last_idx(); i++) {
    //    VPR * v = m_vpr_vec.get(i);
    //    ASSERT0(v != NULL);
    //    stripSpecifiedVP(v);
    //  }
    //Because the information of VPR during striping will not be maintained
    //and the relationship between VPR_orgprno and the concrete occurrence PR
    //may be invalid and that making the process assert.

    for (bblst->get_head(&ct); ct != bblst->end(); ct = bblst->get_next(ct)) {
        IRBB * bb = ct->val();
        IRListIter irct = NULL;
        IRListIter nextirct = NULL;
        for (BB_irlist(bb).get_head(&nextirct), irct = nextirct;
             irct != BB_irlist(bb).end(); irct = nextirct) {
            nextirct = BB_irlist(bb).get_next(nextirct);
            stripStmtVersion(irct->val(), visited);
        }
    }

    END_TIMER(t, "PRSSA: Strip PR version");
}


//Return the replaced one.
static IR * replace_res_pr(IR * stmt,
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
    return stmt;
}


//Strip specified VPR's version.
void PRSSAMgr::stripSpecifiedVP(VPR * vp)
{
    IR * def = SSA_def(vp);
    if (def == NULL) { return; }

    ASSERT0(VPR_version(vp) != 0);

    IR * res = def->getResultPR(vp->orgprno());
    ASSERTN(res, ("Stmt result must be PR%d", vp->orgprno()));

    Type const* newprty = res->getType();
    UINT newprno = m_rg->buildPrno(newprty);

    IR * replaced_one = replace_res_pr(def, vp->orgprno(), newprno, newprty);
    ASSERT0(replaced_one);

    MD const* md = m_rg->genMDforPR(newprno, newprty);
    replaced_one->setRefMD(md, m_rg);
    if (replaced_one->isCallStmt()) {
        //Call stmts may have sideeffect modify MDSet.
        replaced_one->removePRFromUseset(*m_rg->getMiscBitSetMgr(), m_rg);
    } else {
        //PR changed, whereas DU changed.
        //DUSet is useless, free them for other use.
        def->freeDUset(*m_rg->getMiscBitSetMgr());
        replaced_one->cleanRefMDSet();
    }

    SSAUseIter vit = NULL;
    for (INT i = SSA_uses(vp).get_first(&vit);
         vit != NULL; i = SSA_uses(vp).get_next(i, &vit)) {
        IR * use = m_rg->getIR(i);
        ASSERT0(use->is_pr());
        
        //Rename PR.
        PR_no(use) = newprno;

        //Keep the data type of reference unchanged.
        //IR_dt(use) = newprty;

        //PR changed, whereas DU changed.
        //DUSet is useless, free them for other use.
        use->freeDUset(*m_rg->getMiscBitSetMgr());

        //Update MD reference to new PR.
        use->setRefMD(md, m_rg);
    }

    //Set VPR original prno to the new prno to avoid verifyVPR() assertion.
    //However, checking VPR_orgprno after strip_version,
    //I think, is dispensable.
    //Original prno will useful when PHI operand update incrementally.
    VPR_newprno(vp) = newprno;
}


void PRSSAMgr::stripStmtVersion(IR * stmt, xcom::BitSet & visited)
{
    ASSERT0(stmt->is_stmt());
    if (stmt->isWritePR() || stmt->isCallHasRetVal()) {
        VPR * vp = (VPR*)stmt->getSSAInfo();
        ASSERT0(vp);
        if (!visited.is_contain(SSA_id(vp))) {
            ASSERT0(VPR_version(vp) != 0);
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

        VPR * vp = (VPR*)k->getSSAInfo();
        ASSERT0(vp);
        if (!visited.is_contain(SSA_id(vp))) {
            //Version may be zero if there is not any DEF for k.
            //ASSERT0(VPR_version(vp) != 0);

            //MD DU is useless. Free it for other use.
            k->freeDUset(*m_rg->getMiscBitSetMgr());

            //Avoid restriping again.
            visited.bunion(SSA_id(vp));

            stripSpecifiedVP(vp);
        }
    }
}


//Construct DU chain which need by DUMgr.
//This function will build the DUSet for PHI and its USE.
void PRSSAMgr::constructMDDUChainForPR()
{
    for (INT i = 1; i <= m_vpr_vec.get_last_idx(); i++) {
        VPR * v = m_vpr_vec.get(i);
        ASSERT0(v != NULL);
        IR * def = SSA_def(v);
        if (def == NULL) { continue; }

        ASSERT0(def->is_stmt());

        SSAUseIter vit = NULL;
        for (INT i2 = SSA_uses(v).get_first(&vit);
             vit != NULL; i2 = SSA_uses(v).get_next(i2, &vit)) {
            IR * use = m_rg->getIR(i2);
            ASSERT0(use->is_pr());
            ASSERT0(def->isExactDef(use->getRefMD()));
            m_rg->getDUMgr()->buildDUChain(def, use);
        }
    }
}


//Compute SSAInfo for IRs in region that are in SSA mode.
void PRSSAMgr::computeSSAInfo()
{
    reinit();
    BBList * bblst = m_rg->getBBList();
    ASSERT0(bblst);
    if (bblst->get_elem_count() == 0) { return; }

    IRIter ii;
    BBListIter bbct = NULL;
    for (bblst->get_head(&bbct); bbct != bblst->end(); bblst->get_next(&bbct)) {
        IRBB * bb = bbct->val();
        IRListIter irct = NULL;
        for (BB_irlist(bb).get_head(&irct);
             irct != BB_irlist(bb).end(); irct = BB_irlist(bb).get_next(irct)) {
            IR * ir = irct->val();
            ii.clean();
            for (IR * x = iterInit(ir, ii); x != NULL; x = iterNext(ii)) {
                if (x->isReadPR() ||
                    (x->is_stmt() && x->getResultPR() != NULL)) {
                    SSAInfo * ssainfo = allocSSAInfo(x->getPrno());
                    x->setSSAInfo(ssainfo);
                    if (x->isReadPR()) {
                        ssainfo->addUse(x);
                    } else {
                        ASSERTN(SSA_def(ssainfo) == NULL,
                                ("multidefinition in for PR%d", x->getPrno()));
                        SSA_def(ssainfo) = x;
                    }
                }
            }
        }
    }
    m_is_valid = true;
}


void PRSSAMgr::construction(OptCtx & oc)
{
    reinit();
    m_rg->checkValidAndRecompute(&oc, PASS_DOM, PASS_UNDEF);

    //Extract dominate tree of CFG.
    START_TIMER(t, "PRSSA: Extract Dom Tree");
    DomTree domtree;
    m_cfg->get_dom_tree(domtree);
    END_TIMER(t, "PRSSA: Extract Dom Tree");

    if (!construction(domtree)) {
        return;
    }
    m_is_valid = true;
}


//Note: Non-SSA DU Chains of read/write PR is unavaiable after SSA construction.
bool PRSSAMgr::construction(DomTree & domtree)
{
    ASSERT0(m_rg);
    START_TIMER(t, "PRSSA: Build dominance frontier");
    DfMgr dfm;
    dfm.build((xcom::DGraph&)*m_cfg);
    END_TIMER(t, "PRSSA: Build dominance frontier");
    if (dfm.hasHighDFDensityVertex((xcom::DGraph&)*m_cfg)) {
        return false;
    }

    List<IRBB*> wl; //for tmp used
    DefMiscBitSetMgr sm;
    DefSBitSet effect_prs(sm.getSegMgr());
    Vector<DefSBitSet*> defined_prs_vec;
    placePhi(dfm, effect_prs, sm, defined_prs_vec, wl);
    rename(effect_prs, defined_prs_vec, domtree);
    ASSERT0(verifyPhi(true, true) && verifyPRNOofVP());
    refinePhi(wl);

    //Clean version stack after renaming.
    cleanPRNO2Stack();

    //Recompute the map if ssa needs reconstruct.
    m_prno2ir.clean();

    stripVersionForBBList();

    if (g_is_dump_after_pass && g_dump_opt.isDumpPRSSAMgr()) {
        START_TIMER(tdump, "PRSSA: Dump After Pass");
        dump();
        dfm.dump((xcom::DGraph&)*m_cfg);
        END_TIMER(tdump, "PRSSA: Dump After Pass");
    }

    ASSERT0(verifyIRandBB(m_rg->getBBList(), m_rg));
    ASSERT0(verifyPhi(false, false) && verifyVPR());
    m_is_valid = true;
    return true;
}
//END PRSSAMgr


bool PRSSAMgr::verifyPRSSAInfo(Region * rg)
{
    PRSSAMgr * ssamgr = (PRSSAMgr*)(rg->getPassMgr()->
        queryPass(PASS_PR_SSA_MGR));
    if (ssamgr != NULL && ssamgr->is_valid()) {
        ASSERT0(ssamgr->verifySSAInfo());
        ASSERT0(ssamgr->verifyPhi(false, false));
    }
    return true;
}

} //namespace xoc
