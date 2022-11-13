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

static bool haveToMaintainClassicPRDU(OptCtx const& oc)
{
    //After PRSSA, classic PRDU is invalid. However, if classic NonPRDU is
    //still in use, we have to maintain the DU chain for CallStmt, because
    //both PRDU and NonPRDU are recorded in its DUSet.
    //CASE:prssa_duset.c
    return oc.is_nonpr_du_chain_valid();
}


//
//START BB2APRSet
//
void BB2PRSet::addDefBB(PRNO prno, UINT bbid)
{
    PRSet * bs = get(bbid);
    if (bs == nullptr) {
        bs = m_sbsmgr->allocSBitSet();
        set(bbid, bs);
    }
    bs->bunion((BSIdx)prno);
}


PRSet * BB2PRSet::genPRSet(UINT bbid)
{
    PRSet * bs = get(bbid);
    if (bs == nullptr) {
        bs = m_sbsmgr->allocSBitSet();
        set(bbid, bs);
    }
    return bs;
}


void BB2PRSet::genPRSet(BBSet const& bbset)
{
    BBSetIter bbit;
    for (BSIdx i = bbset.get_first(&bbit); i != BS_UNDEF;
         i = bbset.get_next(i, &bbit)) {
        PRSet * bs = get(i);
        if (bs == nullptr) {
            bs = m_sbsmgr->allocSBitSet();
            set(i, bs);
        }
    }
}
//END BB2APRSet


//
//START PRSSARegion
//
//Find PR in 'ir' and add PRs that has 'prno' into irlist.
void PRSSARegion::add(PRNO prno, IR * ir)
{
    IRIter it;
    for (IR * x = iterInit(ir, it); x != nullptr; x = iterNext(it)) {
        if (x->isPROp() && x->getPrno() == prno) {
            add(x);
        }
    }
}


//Find ir into irlist and ir must be PR op.
void PRSSARegion::add(IR * ir)
{
    if (m_idtab.find(ir->id())) { return; }
    m_idtab.append(ir->id());
    m_irlist.append_tail(ir);
    if (ir->is_exp()) {
        m_bbset.bunion(ir->getStmt()->getBB()->id());
    } else {
        m_bbset.bunion(ir->getBB()->id());
    }
}


void PRSSARegion::dump(Region const* rg) const
{
    if (!rg->isLogMgrInit()) { return; }
    note(rg, "\n==-- DUMP PRSSARegion --==");
    ASSERT0(getRootBB());
    note(rg, "\nROOT:BB%d", getRootBB()->id());
    note(rg, "\nBBSET:");
    PRSSARegion * pthis = const_cast<PRSSARegion*>(this);
    pthis->getBBSet().dump(rg->getLogMgr()->getFileHandler());
    note(rg, "\nIRLIST:");
    rg->getLogMgr()->incIndent(2);
    for (IR const* ir = pthis->getIRList().get_head(); ir != nullptr;
         ir = pthis->getIRList().get_next()) {
        dumpIR(ir, rg, nullptr, IR_DUMP_DEF);
    }
    rg->getLogMgr()->decIndent(2);
}
//END PRSSARegion


//
//START BB2VPMap
//
//The allocated object will be destroied at destoryPRNO2VPR().
PRNO2VPR * BB2VPMap::allocPRNO2VPR(UINT bbid)
{
    PRNO2VPR * prno2vp = new PRNO2VPR();
    set(bbid, prno2vp);
    return prno2vp;
}


void BB2VPMap::destoryPRNO2VPR(UINT bbid)
{
    PRNO2VPR * prno2vp = get(bbid);
    ASSERT0(prno2vp);
    set(bbid, nullptr);
    delete prno2vp;
}


//Verify if vpmap of each BB has been deleted.
bool BB2VPMap::verify()
{
    for (VecIdx i = 0; i <= get_last_idx(); i++) {
        ASSERT0(get(i) == nullptr);
    }
    return true;
}
//END BB2VPMap


//
//START DfMgr
//
//Get the BB set where 'v' is the dominate frontier of them.
xcom::BitSet * DfMgr::genDFControlSet(UINT vid)
{
    xcom::BitSet * df = m_df_vec.get(vid);
    if (df == nullptr) {
        df = m_bs_mgr.create();
        m_df_vec.set(vid, df);
    }
    return df;
}


void DfMgr::clean()
{
    for (VecIdx i = 0; i <= m_df_vec.get_last_idx(); i++) {
        xcom::BitSet * df = m_df_vec.get(i);
        if (df != nullptr) {
            df->clean();
        }
    }
}


//g: dump dominance frontier to graph.
void DfMgr::dump(xcom::DGraph const& g, Region * rg) const
{
    if (!rg->isLogMgrInit()) { return; }
    note(rg, "\n==---- DUMP Dominator Frontier Control Set ----==\n");
    VertexIter c;
    for (xcom::Vertex const* v = g.get_first_vertex(c);
         v != nullptr; v = g.get_next_vertex(c)) {
        UINT vid = v->id();
        note(rg, "\nBB%d DF set:", vid);
        xcom::BitSet const* df = m_df_vec.get(vid);
        if (df != nullptr) {
            for (BSIdx i = df->get_first(); i != BS_UNDEF;
                 i = df->get_next(i)) {
                prt(rg, "%d,", i);
            }
        }
    }
}


//This function compute dominance frontier to graph g.
void DfMgr::buildRecur(xcom::Vertex const* v, xcom::DGraph const& g,
                       DomTree const& domtree)
{
    UINT vid = v->id();
    xcom::Vertex * v_domtree = domtree.getVertex(vid);
    ASSERT0(v_domtree);

    //Access each succs.
    for (xcom::EdgeC const* ec = v_domtree->getOutList();
         ec != nullptr; ec = EC_next(ec)) {
        xcom::Vertex const* succ_domtree = ec->getTo();
        xcom::Vertex const* succ = g.getVertex(succ_domtree->id());
        buildRecur(succ, g, domtree);
    }

    xcom::BitSet * df = genDFControlSet(vid);
    df->clean();

    //Compute DF(local)
    for (xcom::EdgeC const* ec = v->getOutList();
         ec != nullptr; ec = EC_next(ec)) {
        xcom::Vertex const* succ = ec->getTo();
        if (g.get_idom(succ->id()) != vid) {
            df->bunion(succ->id());
        }
    }

    //Compute DF(up)
    for (xcom::EdgeC const* ec = v_domtree->getOutList();
         ec != nullptr; ec = EC_next(ec)) {
        xcom::Vertex const* succ_domtree = ec->getTo();
        xcom::BitSet * succ_df = genDFControlSet(succ_domtree->id());
        for (BSIdx p = succ_df->get_first(); p != BS_UNDEF;
             p = succ_df->get_next(p)) {
            if (g.get_idom((UINT)p) != vid) {
                df->bunion(p);
            }
        }
    }
}


//This function compute dominance frontier to graph g recursively.
void DfMgr::build(xcom::DGraph const& g, DomTree const& domtree)
{
    VertexIter c;
    for (xcom::Vertex const* v = g.get_first_vertex(c);
         v != nullptr; v = g.get_next_vertex(c)) {
        buildRecur(v, g, domtree);
    }
}


//Count Dominator Frontier Density for each xcom::Vertex.
//Return true if there exist vertex that might inserting
//ton of PHIs which will blow up memory.
bool DfMgr::hasHighDFDensityVertex(xcom::DGraph const& g)
{
    Vector<UINT> counter_of_vex(g.getVertexNum());
    VertexIter c;
    for (xcom::Vertex const* v = g.get_first_vertex(c);
         v != nullptr; v = g.get_next_vertex(c)) {
        xcom::BitSet const* dfset = getDFControlSet(v->id());
        if (dfset == nullptr) { continue; }
        for (BSIdx i = dfset->get_first(); i != BS_UNDEF;
             i = dfset->get_next(i)) {
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
    VertexIter c;
    for (xcom::Vertex const* v = g.get_first_vertex(c);
         v != nullptr; v = g.get_next_vertex(c)) {
        xcom::BitSet const* v_dom = g.get_dom_set(v->id());
        ASSERT0(v_dom != nullptr);
        UINT vid = v->id();

        //Access each preds
        AdjVertexIter it;
        for (Vertex const* pred = Graph::get_first_in_vertex(v, it);
             pred != nullptr; pred = Graph::get_next_in_vertex(it)) {
            xcom::BitSet * pred_df = genDFControlSet(pred->id());
            if (pred == v || g.get_idom(vid) != pred->id()) {
                pred_df->bunion(vid);
            }
            xcom::BitSet const* pred_dom = g.get_dom_set(pred->id());
            ASSERT0(pred_dom != nullptr);
            for (BSIdx i = pred_dom->get_first();
                 i != BS_UNDEF; i = pred_dom->get_next(i)) {
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
//START PRSSAInfoCollect
//
void PRSSAInfoCollect::init(PRSSAMgr * mgr, OptCtx const& oc)
{
    ASSERT0(mgr && mgr->is_valid());
    m_ssamgr = mgr;
    m_rg = mgr->getRegion();
    m_cfg = m_rg->getCFG();
    m_oc = &oc;
}


bool PRSSAInfoCollect::isDomLiveinBBFromPred(UINT bb, UINT pred, UINT meetup)
{
    ASSERT0(bb != BBID_UNDEF && meetup != BBID_UNDEF && pred != BBID_UNDEF);
    //TODO:Check the algo.
    //DomSet const* ds = m_cfg->gen_dom_set(pred);
    //if (ds->is_contain(bb)) { return false; }
    //if (ds->is_contain(meetup)) { return true; }
    //return false;
    xcom::List<UINT> wl;
    wl.append_tail(pred);
    xcom::TTab<UINT> visited;
    while (wl.get_elem_count() != 0) {
        UINT t = wl.remove_head();
        if (t == meetup) { return true; }
        if (t == bb) { return false; }
        visited.append(t);
        UINT dom = ((DGraph*)m_cfg)->get_idom(t);
        if (dom == VERTEX_UNDEF) { continue; }

        ASSERTN(m_cfg->isVertex(dom), ("miss DomInfo"));
        if (!visited.find(dom)) {
            wl.append_tail(dom);
        }
    }
    return false;
}


IR const* PRSSAInfoCollect::getLiveinRef(IR const* phi) const
{
    ASSERT0(phi->is_phi());
    return m_phi2livein.get(phi);
}


void PRSSAInfoCollect::collect(IRBB const* from, IRBB const* to)
{
    ASSERT0(m_oc->is_dom_valid());
    BBIRListIter it;
    for (IR const* ir = const_cast<IRBB*>(to)->getIRList().get_head(&it);
         ir != nullptr; ir = const_cast<IRBB*>(to)->getIRList().get_next(&it)) {
        if (!ir->is_phi()) { return; }
        if (from == to) {
            m_phi2livein.set(ir, ir);
            continue;
        }
        EdgeC const* ec = to->getVex()->getInList();
        //In the middle stage of optimization, e.g DCE, may transform the CFG
        //into a legal but insane CFG. In the case, ec and Phi operand may be
        //empty.
        for (IR const* opnd = PHI_opnd_list(ir); opnd != nullptr;
             opnd = opnd->get_next(), ec = ec->get_next()) {
            ASSERT0(ec);
            if (isDomLiveinBBFromPred(to->id(), ec->getFromId(), from->id())) {
                m_phi2livein.set(ir, opnd);
                break;
            }
        }
    }
}
//END PRSSAInfoCollect


//
//START SSAGraph
//
SSAGraph::SSAGraph(Region * rg, PRSSAMgr * ssamgr) :
    m_rg(rg), m_ssa_mgr(ssamgr)
{
    ASSERT0(rg && ssamgr);
    m_ssa_mgr = ssamgr;
    VPRVec const* vpr_vec = ssamgr->getVPRVec();
    UINT inputcount = 1;
    for (VecIdx i = 1; i <= vpr_vec->get_last_idx(); i++) {
        VPR * v = vpr_vec->get(i);
        ASSERT0(v != nullptr);
        IR * def = SSA_def(v);
        if (def == nullptr) {
            ASSERT0(VPR_version(v) == PRSSA_INIT_VERSION);
            UINT vdef = 0xffffFFFF - inputcount;
            inputcount++;
            addVertex(vdef);
            m_vdefs.set(vdef, v);

            //May be input parameters.
            SSAUseIter vit = nullptr;
            for (BSIdx i2 = SSA_uses(v).get_first(&vit);
                 vit != nullptr; i2 = SSA_uses(v).get_next(i2, &vit)) {
                IR * use = m_rg->getIR(i2);
                ASSERT0(use->is_pr());
                addEdge(vdef, IR_id(use->getStmt()));
            }
        } else {
            ASSERT0(def->is_stmt());
            addVertex(def->id());
            SSAUseIter vit = nullptr;
            for (BSIdx i2 = SSA_uses(v).get_first(&vit);
                 vit != nullptr; i2 = SSA_uses(v).get_next(i2, &vit)) {
                IR * use = m_rg->getIR(i2);
                ASSERT0(use->is_pr());
                addEdge(def->id(), IR_id(use->getStmt()));
            }
        }
    }
}


void SSAGraph::dump(CHAR const* name, bool detail) const
{
    if (name == nullptr) {
        name = "graph_ssa_graph.vcg";
    }
    UNLINK(name);
    FILE * h = fopen(name, "a+");
    ASSERTN(h != nullptr, ("%s create failed!!!",name));

    //Print comment
    fprintf(h, "\n/*");
    m_rg->getLogMgr()->push(h, name);
    dumpBBList(m_rg->getBBList(), m_rg);
    fprintf(h, "\n*/\n");

    //Print graph structure description.
    fprintf(h,
            "graph: {"
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
    List<IR const*> lst;
    VertexIter itv = VERTEX_UNDEF;
    for (xcom::Vertex * v = get_first_vertex(itv);
         v != nullptr; v = get_next_vertex(itv)) {
        VPR * vp = m_vdefs.get(v->id());
        if (vp != nullptr) {
            //Print virtual def for parameter.
            fprintf(h,
                    "\nnode: { title:\"%u\" shape:hexagon fontname:\"courB\" "
                    "color:lightgrey label:\" param:P%uV%uP%u \"}",
                    v->id(), vp->orgprno(), vp->version(), vp->newprno());
            continue;
        }

        IR * def = m_rg->getIR(v->id());
        ASSERT0(def != nullptr);
        IR * res = def->getResultPR();
        if (res != nullptr) {
            fprintf(h, "\nnode: { title:\"%d\" shape:box fontname:\"courB\" "
                        "color:gold label:\"", def->id());
            for (IR * r = res; r != nullptr; r = r->get_next()) {
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
             opnd != nullptr; opnd = iterNextC(lst)) {
             if (!def->is_rhs(opnd) || !opnd->is_pr()) {
                 continue;
             }
             VPR * use_vp = (VPR*)PR_ssainfo(opnd);
             fprintf(h, "%s%uv%u%s%u, ", PR_TYPE_CHAR, use_vp->orgprno(),
                     use_vp->version(), PR_TYPE_CHAR, use_vp->newprno());
        }
        if (detail) {
            //TODO: implement dump_ir_buf();
            dumpIR(def, m_rg, nullptr, IR_DUMP_KID);
        }
        fprintf(h, "\"}");
    }

    //Print edge
    EdgeIter c;
    for (xcom::Edge * e = get_first_edge(c);
         e != nullptr; e = get_next_edge(c)) {
        xcom::Vertex * from = EDGE_from(e);
        xcom::Vertex * to = EDGE_to(e);
        fprintf(h, "\nedge: { sourcename:\"%u\" targetname:\"%u\" %s}",
                from->id(), to->id(),  "");
    }
    fprintf(h, "\n}\n");
    m_rg->getLogMgr()->pop();

    fclose(h);
}
//END SSAGraph



//
//START PRSSAMgr
//
size_t PRSSAMgr::count_mem() const
{
    size_t count = 0;
    count += smpoolGetPoolSize(m_vp_pool);
    count += m_prno2vprvec.count_mem();
    count += m_prno2stack.count_mem();
    count += sizeof(m_vp_count);
    count += m_vpr_vec.count_mem();
    count += m_prno2vpr.count_mem();
    count += m_prno2maxversion.count_mem();
    count += m_prno2type.count_mem();
    count += sizeof(m_rg);
    return count;
}


void PRSSAMgr::setVPR(PRNO prno, VPR * vp)
{
    m_prno2vpr.set((VecIdx)prno, vp);
}


//Clean version stack.
void PRSSAMgr::cleanPRNO2VPRStack()
{
    for (VecIdx i = 0; i <= m_prno2stack.get_last_idx(); i++) {
        Stack<VPR*> * s = m_prno2stack.get(i);
        if (s != nullptr) { delete s; }
    }
    m_prno2stack.clean();
}


void PRSSAMgr::cleanPRNO2MaxVersion()
{
    m_prno2maxversion.clean();
}


void PRSSAMgr::cleanPRNO2Type()
{
    m_prno2type.clean();
}

//Dump ssa du stmt graph.
void PRSSAMgr::dumpSSAGraph(CHAR const* name) const
{
    SSAGraph sa(m_rg, const_cast<PRSSAMgr*>(this));
    sa.dump(name, true);
}


CHAR * PRSSAMgr::dumpVPR(VPR const* v, OUT CHAR * buf) const
{
    sprintf(buf, "%s%uV%u%s%u", PR_TYPE_CHAR, v->orgprno(), v->version(),
            PR_TYPE_CHAR, v->newprno());
    return buf;
}


//This function dumps VPR structure and SSA DU info.
//have_renamed: set true if PRs have been renamed in construction.
void PRSSAMgr::dumpAllVPR() const
{
    if (!m_rg->isLogMgrInit()) { return; }
    note(getRegion(), "\n==---- DUMP PRSSAMgr:VPR '%s' ----==",
         m_rg->getRegionName());
    VPRVec const* vpr_vec = const_cast<PRSSAMgr*>(this)->getVPRVec();
    for (VecIdx i = PRNO_UNDEF + 1; i <= vpr_vec->get_last_idx(); i++) {
        VPR * v = vpr_vec->get(i);
        ASSERT0(v != nullptr);
        v->dump(getRegion());
    }
}

VPR * PRSSAMgr::allocVPRImpl(PRNO orgprno, PRNO newprno, UINT version,
                             Type const* orgtype, VPRVec * vprvec)
{
    ASSERTN(m_seg_mgr, ("SSA manager is not initialized"));
    VPR * v = allocVPR();
    if (vprvec != nullptr) {
        vprvec->set(version, v);
    }
    v->initNoClean(m_seg_mgr);
    VPR_orgprno(v) = orgprno;
    VPR_newprno(v) = newprno;
    VPR_orgpr_type(v) = orgtype;
    VPR_version(v) = version;
    SSA_def(v) = nullptr;
    SSA_id(v) = m_vp_count++;
    m_vpr_vec.set(SSA_id(v), v);
    return v;
}


//Allocate SSAInfo and ensure it is unique according to 'version' and 'prno'.
//prno: the function will generate SSAInfo for the prno.
//version: expect version of given prno.
//type: data type of prno.
SSAInfo * PRSSAMgr::allocSSAInfo(PRNO prno, Type const* type)
{
    ASSERT0(prno != PRNO_UNDEF);
    ASSERT0(getSSAInfoByPRNO(prno) == nullptr);
    SSAInfo * ssainfo = allocVPRImpl(prno, prno, PRSSA_INIT_VERSION + 1,
                                     type, nullptr);
    setVPR(prno, (VPR*)ssainfo);
    return ssainfo;
}


//Allocate VPR and ensure it is unique according to 'version' and 'orgprno'.
//orgprno: describ the PRNO that expect to be versioned.
//version: current version of Versioned PR
//orgtype: data type of orginal prno
VPR * PRSSAMgr::allocVPR(PRNO orgprno, UINT version, Type const* orgtype)
{
    ASSERT0(orgprno != PRNO_UNDEF);
    VPRVec * vec = m_prno2vprvec.get((VecIdx)orgprno);
    if (vec == nullptr) {
        vec = new VPRVec();
        m_prno2vprvec.set((VecIdx)orgprno, vec);
    }
    VPR * v = vec->get(version);
    if (v != nullptr) {
        return v;
    }
   return allocVPRImpl(orgprno, PRNO_UNDEF, version, orgtype, vec);
}


//Build Def-Use chain for 'def' and 'use'.
//def: def stmt that writes PR.
//use: use expression that reads PR.
//Note caller should guarrentee 'use' does not belong to other Def-Use chain.
//Note the function does NOT check the consistency of Prno if def or use
//operate on PR.
void PRSSAMgr::buildDUChain(MOD IR * def, MOD IR * use)
{
    ASSERT0(def->isWritePR() || def->isCallHasRetVal());
    ASSERT0(use->isReadPR());
    //TBD:Do def and use have to be same Prno?
    ASSERT0(def->getPrno() == use->getPrno());
    SSAInfo * defssainfo = def->getSSAInfo();
    if (defssainfo == nullptr) {
        defssainfo = allocSSAInfo(def->getPrno(), def->getType());
        def->setSSAInfo(defssainfo);
        SSA_def(defssainfo) = def;
    } else {
        ASSERT0(defssainfo->getDef() == def);
    }

    //Apply new SSAInfo directly, it will displace original ssainfo.
    //You may be set multiple defs for use.
    //Or you should removeSSAUse for original 'use' first.
    if (use->getSSAInfo() != nullptr) {
        ASSERTN(use->getSSAInfo() == defssainfo, ("already has SSA info."));
        ASSERT0(use->getSSAInfo()->findUse(use));
        return;
    }

    use->setSSAInfo(defssainfo);
    defssainfo->addUse(use);
}


//is_reinit: this function is invoked in reinit().
void PRSSAMgr::destroy(bool is_reinit)
{
    if (m_vp_pool == nullptr) { return; }

    //CAUTION: if you do not destruct SSA prior to destory().
    //The reference to IR's SSAInfo will lead to undefined behaviors.
    //ASSERTN(!is_valid(), ("Still in ssa mode, you should out of "
    //                      "SSA before the destruction."));

    for (VecIdx i = 0; i <= m_prno2vprvec.get_last_idx(); i++) {
        VPRVec * vpv = m_prno2vprvec.get((UINT)i);
        if (vpv != nullptr) { delete vpv; }
    }

    cleanPRNO2VPRStack();

    for (VecIdx i = 0; i <= m_vpr_vec.get_last_idx(); i++) {
        VPR * v = m_vpr_vec.get((UINT)i);
        if (v != nullptr) {
            v->destroy();
        }
    }

    if (is_reinit) {
        m_prno2vprvec.clean();
        m_vpr_vec.clean();
        m_prno2vpr.clean();
        m_prno2maxversion.clean();
        m_prno2type.clean();
    }

    removePhiList();

    //Do not free irs in m_prno2type.
    smpoolDelete(m_vp_pool);
    m_vp_pool = nullptr;
}


static void dumpBriefStmt(IR const* ir, Region const* rg)
{
    ASSERT0(ir->is_stmt());
    note(rg, "\n");
    IR * res = const_cast<IR*>(ir)->getResultPR();
    if (res != nullptr) {
        VPR * vp = (VPR*)res->getSSAInfo();
        if (vp == nullptr) {
            prt(rg, "NOSSAINFO!!");
        } else {
            prt(rg, "%s%d", PR_TYPE_CHAR, res->getPrno());
        }
    } else {
        prt(rg, "--");
    }
    prt(rg, " <= ");
    ConstIRIter it;
    bool first = true;
    bool find = false;
    for (IR const* opnd = xoc::iterInitC(ir, it);
         opnd != nullptr; opnd = xoc::iterNextC(it)) {
        if (!opnd->is_pr()) { continue; }
        if (!first) { prt(rg, ","); }
        VPR * vp = (VPR*)PR_ssainfo(opnd);
        if (vp == nullptr) {
            prt(rg, "NOSSAINFO!!");
        } else {
            prt(rg, "%s%d", PR_TYPE_CHAR, opnd->getPrno());
        }
        find = true;
        first = false;
    }
    if (!find) {
        prt(rg, "--");
    }
}


//The function dump PR info rather than specific IR stmt and exp details.
bool PRSSAMgr::dumpBrief() const
{
    if (!m_rg->isLogMgrInit()) { return false; }
    note(getRegion(), "\n==---- DUMP BRIEF %s '%s' ----==",
         getPassName(), m_rg->getRegionName());
    dumpAllVPR();
    note(getRegion(), "\n==---- DUMP PRSSAMgr:VPR REF '%s' ----==",
         m_rg->getRegionName());
    BBList * bbl = m_cfg->getBBList();
    for (IRBB * bb = bbl->get_head(); bb != nullptr; bb = bbl->get_next()) {
        bb->dumpDigest(getRegion());
        for (IR * ir = BB_first_ir(bb); ir != nullptr; ir = BB_next_ir(bb)) {
            dumpBriefStmt(ir, getRegion());
        }
    }
    return true;
}


bool PRSSAMgr::dump() const
{
    if (!m_rg->isLogMgrInit()) { return false; }
    note(getRegion(), "\n==---- DUMP %s '%s' ----==",
         getPassName(), m_rg->getRegionName());
    dumpAllVPR();
    note(getRegion(), "\n==---- DUMP PRSSAMgr:VPR REF '%s' ----==",
         m_rg->getRegionName());
    BBList * bbl = m_cfg->getBBList();
    List<IR const*> lst;
    List<IR const*> opnd_lst;
    for (IRBB * bb = bbl->get_head(); bb != nullptr; bb = bbl->get_next()) {
        bb->dumpDigest(getRegion());
        for (IR * ir = BB_first_ir(bb);
             ir != nullptr; ir = BB_next_ir(bb)) {
            m_rg->getLogMgr()->incIndent(4);
            note(getRegion(), "\n------------------");
            dumpIR(ir, m_rg);
            lst.clean();
            opnd_lst.clean();
            for (IR const* opnd = iterInitC(ir, lst);
                opnd != nullptr;
                opnd = iterNextC(lst)) {
                if (ir->is_rhs(opnd) && opnd->is_pr()) {
                    opnd_lst.append_tail(opnd);
                }
            }

            IR * res = ir->getResultPR();
            note(getRegion(), "\nVP:");
            if (res != nullptr) {
                VPR * vp = (VPR*)res->getSSAInfo();
                if (vp == nullptr) {
                    prt(getRegion(), "NOSSAINFO!!");
                } else {
                    prt(getRegion(), "%s%dv%d%s%d ",
                        PR_TYPE_CHAR, vp->orgprno(),
                        vp->version(), PR_TYPE_CHAR, vp->newprno());
                }
            } else {
                prt(getRegion(), "--");
            }
            prt(getRegion(), " <= ");
            if (opnd_lst.get_elem_count() != 0) {
                UINT i = 0, n = opnd_lst.get_elem_count() - 1;
                for (IR const* opnd = opnd_lst.get_head(); opnd != nullptr;
                     opnd = opnd_lst.get_next(), i++) {
                    VPR * vp = (VPR*)PR_ssainfo(opnd);
                    if (vp == nullptr) {
                        prt(getRegion(), "NOSSAINFO!!");
                    } else {
                        prt(getRegion(), "%s%dv%d%s%d",
                            PR_TYPE_CHAR, vp->orgprno(),
                            vp->version(), PR_TYPE_CHAR, vp->newprno());
                    }
                    if (i < n) { prt(getRegion(), ","); }
                }
            } else {
                prt(getRegion(), "--");
            }
            m_rg->getLogMgr()->decIndent(4);
        }
    }
    return true;
}


//Initialize VPR and Type for each PR.
//ir can be stmt or expression.
//Note the function record the type that first met as the initial type of
//specific Prno.
void PRSSAMgr::initVPR(MOD IR * ir, PRSet const* prset)
{
    IR * res = nullptr;
    if (ir->is_stmt()) {
        res = ir->getResultPR();
    }
    //Process result.
    if (res != nullptr) {
        PRNO prno = res->getPrno();
        if (prset == nullptr ||
            prset != nullptr && prset->is_contain((BSIdx)prno)) {
            ir->setSSAInfo(allocVPR(prno, PRSSA_INIT_VERSION, res->getType()));
            if (m_prno2type.get((VecIdx)prno) == nullptr) {
                m_prno2type.set((VecIdx)prno, ir->getType());
            }
        }
    }
    //Process opnd.
    m_iter.clean();
    for (IR * kid = xoc::iterInit(ir, m_iter);
         kid != nullptr; kid = xoc::iterNext(m_iter)) {
        if (!kid->isReadPR()) { continue; }
        PRNO kidno = kid->getPrno();
        if (prset != nullptr && !prset->is_contain((BSIdx)kidno)) { continue; }
        kid->setSSAInfo(allocVPR(kidno, PRSSA_INIT_VERSION,
                                 kid->getType()));
        if (m_prno2type.get((VecIdx)kidno) == nullptr) {
            m_prno2type.set((VecIdx)kidno, kid->getType());
        }
    }
}


bool PRSSAMgr::isLiveOut(PRNO prno, UINT bbid)
{
    ASSERT0(m_livemgr);
    return m_livemgr->get_liveout(bbid)->is_contain(prno);
}


//definedprs: record PRs which defined in 'bb'.
void PRSSAMgr::collectPRAndInitVPRForBB(IRBB const* bb, OUT PRSet & definedprs)
{
    BBIRList const& lst = const_cast<IRBB*>(bb)->getIRList();
    BBIRListIter it;
    for (IR * ir = lst.get_head(&it); ir != nullptr; ir = lst.get_next(&it)) {
        //Generate VPR for non-phi stmt.
        ASSERT0(!ir->is_phi());
        initVPR(ir, nullptr);
        if (ir->getResultPR() == nullptr) { continue; }
        ASSERT0(ir->getResultPR()->is_single());
        PRNO prno = ir->getPrno();
        definedprs.bunion((BSIdx)prno);
    }
}


IR * PRSSAMgr::insertPhi(PRNO prno, MOD IRBB * bb)
{
    Type const* prtype = mapPRNO2Type(prno);
    ASSERT0(prtype);
    UINT num_opnd = bb->getVex()->getInDegree();
    ASSERT0(num_opnd > 0);

    //Here each operand and result of phi set to same type.
    //They will be revised to correct type during renaming.
    IR * phi = m_rg->getIRMgr()->buildPhi(prno, prtype, num_opnd);
    m_rg->getMDMgr()->allocRef(phi);
    for (IR * opnd = PHI_opnd_list(phi);
         opnd != nullptr; opnd = opnd->get_next()) {
        opnd->copyRef(phi, m_rg);
    }
    BB_irlist(bb).append_head(phi);
    initVPR(phi, nullptr);
    return phi;
}


void PRSSAMgr::addUse(IR * to, IR const* from)
{
    ASSERT0(to->isPROp() && from->isPROp());
    SSAInfo * ssainfo = from->getSSAInfo();
    ASSERT0(ssainfo);
    PR_ssainfo(to) = ssainfo;
    ssainfo->addUse(to);
}


void PRSSAMgr::addUseForTree(IR * to, IR const* from)
{
    if (to == from) { return; }
    ASSERT0(to->is_exp() && from->isPROp());
    SSAInfo * ssainfo = from->getSSAInfo();
    ASSERT0(ssainfo);
    IRIter it;
    for (IR * to_ir = xoc::iterInit(to, it, false);
         to_ir != nullptr; to_ir = xoc::iterNext(it)) {
        if (to_ir->isPROp()) {
            PR_ssainfo(to) = ssainfo;
            ssainfo->addUse(to);
        }
    }
}


//After adding BB or change BB successor,
//you need add the related PHI operand if BB successor has PHI stmt.
void PRSSAMgr::addSuccessorDesignatedPhiOpnd(IRBB * bb, IRBB * succ,
                                             PRSSAInfoCollect const& col)
{
    UINT pos = m_cfg->WhichPred(bb, succ);
    IRListIter it;
    for (IR * ir = BB_irlist(succ).get_head(&it);
         ir != nullptr; ir = BB_irlist(succ).get_next(&it)) {
        if (!ir->is_phi()) { break; }
        IR const* ref = col.getLiveinRef(ir);
        if (ref != nullptr) {
            if (ref->isPROp()) {
                SSAInfo * opnd_ssainfo = ref->getSSAInfo();
                ASSERT0(opnd_ssainfo);
                IR * newopnd = CPr::dupIRTreeByRef(ref, m_rg);
                PRSSAMgr::addUse(newopnd, ref);
                ((CPhi*)ir)->insertOpndAt(pos, newopnd);
                continue;
            }
            ASSERT0(ref->isPhiOpnd());
            ((CPhi*)ir)->insertOpndAt(pos, m_rg->dupIRTree(ref));
            continue;
        }
        //In the middle stage of optimization, e.g DCE, may transform the CFG
        //into a legal but insane CFG. In the case, the CFG may be divided
        //into serveral isolated parts. Thus there is no livein path from entry
        //to current bb.
        //ASSERTN(col.getOptCtx()->is_dom_valid(), ("invalid dominfo"));
        IR * newopnd = m_rg->getIRMgr()->buildPR(ir->getType());
        m_rg->getMDMgr()->allocRef(newopnd);
        genSSAInfoForExp(newopnd);
        ((CPhi*)ir)->insertOpndAt(pos, newopnd);
    }
}


//Insert phi for PR.
//defbbs: record a list of BB that defined given PR identified by 'prno'.
void PRSSAMgr::placePhiForPR(PRNO prno, List<IRBB*> const* defbbs,
                             DfMgr const& dfm, xcom::BitSet & visited,
                             List<IRBB*> & wl, BB2PRSet const& bb2definedprs,
                             BBSet const* bbset)
{
    ASSERTN(defbbs, ("PR that without any DEF should not get to here."));
    if (m_is_semi_pruned && defbbs->get_elem_count() <= 1) { return; }
    visited.clean();
    wl.clean();
    C<IRBB*> * it;
    for (IRBB * defbb = defbbs->get_head(&it);
         defbb != nullptr; defbb = defbbs->get_next(&it)) {
        wl.append_tail(defbb);
        //visited.bunion(defbb->id());
    }
    while (wl.get_elem_count() != 0) {
        IRBB * defbb = wl.remove_head();
        if (m_is_pruned && !isLiveOut(prno, defbb->id())) {
            continue;
        }

        //Each BB in Set dfcs is in dominance frontier of 'defbb'.
        xcom::BitSet const* dfcs = dfm.getDFControlSet(defbb->id());
        if (dfcs == nullptr) { continue; }

        for (BSIdx i = dfcs->get_first(); i != BS_UNDEF;
             i = dfcs->get_next(i)) {
            if (visited.is_contain(i)) {
                //Already insert phi for 'prno' into BB i.
                //TODO:ensure the phi for same PR does NOT be
                //inserted multiple times.
                continue;
            }
            if (bbset != nullptr && !bbset->is_contain(i)) {
                continue;
            }
            visited.bunion(i);
            IRBB * ibb = m_cfg->getBB(i);
            ASSERT0(ibb);

            //Redundant phi will be removed during refinePhi().
            insertPhi(prno, ibb);
            PRSet * prset = bb2definedprs.get(i);
            ASSERT0(prset);
            prset->bunion((BSIdx)prno); //Add phi to defined PRSet.
            wl.append_tail(ibb);
        }
    }
}


//Return true if all DEF of operands are the same stmt.
//common_def: record the same stmt.
//            Note if the same stmt indicates a region livein DEF, it will be
//            NULL.
//common_livein: record the livein PR if all opnd are the same livein PR.
static bool hasCommonDef(IR const* phi, OUT IR ** common_def,
                         OUT IR ** common_livein)
{
    ASSERT0(phi->is_phi());
    IR * liveinopnd = nullptr;
    IR * effectdef = nullptr;
    bool same_def = true; //indicate all DEF of operands are the same stmt.
    for (IR * opnd = PHI_opnd_list(phi);
         opnd != nullptr; opnd = opnd->get_next()) {
        ASSERT0(opnd->isPhiOpnd());
        if (!opnd->is_pr()) {
            //CASE: phi=(10, opnd0)
            //Note we leave the redundant case:phi=(10) to IR Refinement.
            return false;
        }

        SSAInfo const* opndsi = PR_ssainfo(opnd);
        ASSERT0(opndsi);
        IR * opndsidef = SSA_def(opndsi);
        if (opndsidef == nullptr) {
            //Note opndsidef is NULL if opnd is livein PR.
            if (liveinopnd == nullptr) {
                liveinopnd = opnd;
            } else if (liveinopnd->getPrno() != opnd->getPrno()) {
                same_def = false;
                break;
            }
            continue;
        }
        if (effectdef == nullptr) {
            if (opndsidef == phi) {
                continue;
            }
            effectdef = opndsidef;
            continue;
        }
        ASSERT0(effectdef != phi);
        if (effectdef != opndsidef && phi != opndsidef) {
            same_def = false;
            break;
        }
    }
    ASSERT0(common_def && (effectdef || liveinopnd));
    *common_def = effectdef == nullptr ? nullptr : effectdef;
    *common_livein = liveinopnd == nullptr ? nullptr : liveinopnd;
    if ((*common_def) != nullptr && (*common_livein) != nullptr) {
        //CASE: phi $3 = (livein $1), (defined $2), (defined $2), (defined $2)
        //There is no commond-def of phi's operand.
        same_def = false;
    }
    return same_def;
}


//Return true if phi is redundant, otherwise return false.
//If all opnds have same defintion or defined by current phi,
//the phi is redundant.
//common_def: record the common DEF stmt if the definition of all opnd is
//            the same stmt.
//common_livein: record the common livein PR if all opnd are the same livein PR.
//TODO: p=phi(m,p), the only use of p is phi. the phi is redundant.
static bool isRedundantPHI(IR const* phi, OUT IR ** common_def,
                           OUT IR ** common_livein)
{
    VPR * vp = (VPR*)PHI_ssainfo(phi);
    ASSERT0(vp);
    if (SSA_uses(vp).get_elem_count() == 0) { return true; }
    return hasCommonDef(phi, common_def, common_livein);
}


void PRSSAMgr::initMapInfo(DefMiscBitSetMgr & bs_mgr,
                           OUT BB2PRSet & bb2definedprs,
                           OUT PRSet & prset)
{
    START_TIMER(t, "PRSSA: Init Map Info");
    //Record modified PR for each BB.
    BBList * bblst = m_cfg->getBBList();
    for (IRBB * bb = bblst->get_head(); bb != nullptr; bb = bblst->get_next()) {
        PRSet * bbprs = bb2definedprs.genPRSet(bb->id());
        collectPRAndInitVPRForBB(bb, *bbprs);

        //Regard all defined PR as effect, and they will be versioned later.
        prset.bunion(*bbprs);
    }
    END_TIMER(t, "PRSSA: Init Map Info");
}


//Place phi and assign the v0 for each PR.
//prset: record set of PR which need to version.
void PRSSAMgr::placePhiHelper(DfMgr const& dfm, PRSet const& prset,
                              BBSet const* bbset,
                              PR2DefBBSet const& pr2defbbset,
                              BB2PRSet const& bb2definedprs)
{
    //Place phi for lived effect prs.
    xcom::BitSet visited; //for tmp used
    List<IRBB*> wl; //for tmp used
    DefSBitSetIter cur = nullptr;
    for (BSIdx i = prset.get_first(&cur); i != BS_UNDEF;
         i = prset.get_next(i, &cur)) {
        ASSERT0(i != PRNO_UNDEF);
        List<IRBB*> const* defbbs = pr2defbbset.get(i);
        ASSERTN(defbbs, ("PR that without any DEF should not get to here."));
        placePhiForPR((PRNO)i, defbbs, dfm, visited, wl, bb2definedprs, bbset);
    }
}


//Place phi and assign the v0 for each PR.
//prset: record set of PR which need to version.
void PRSSAMgr::placePhi(DfMgr const& dfm, PRSet const& prset,
                        BBList const& bblist, BB2PRSet const& bb2definedprs)
{
    START_TIMER(t, "PRSSA: Place Phi");
    //Place phi for lived effect prs.
    PR2DefBBSet pr2defbbset(&bblist, bb2definedprs);
    placePhiHelper(dfm, prset, nullptr, pr2defbbset, bb2definedprs);
    END_TIMER(t, "PRSSA: Place Phi");
}


//Place phi and assign the v0 for each PR.
//prset: record set of PR which need to version.
void PRSSAMgr::placePhi(DfMgr const& dfm, PRSet const& prset,
                        BBSet const& bbset, BB2PRSet const& bb2definedprs)
{
    START_TIMER(t, "PRSSA: Place Phi");
    //Place phi for lived effect prs.
    PR2DefBBSet pr2defbbset(bbset, bb2definedprs, m_rg);
    placePhiHelper(dfm, prset, &bbset, pr2defbbset, bb2definedprs);
    END_TIMER(t, "PRSSA: Place Phi");
}


//Rename opnd, except PHI.
//Walk through RHS expression of 'ir' to rename PR's VPR.
void PRSSAMgr::renameRHS(MOD IR * ir, PRSet const* prset)
{
    ASSERT0(ir->is_stmt() && !ir->is_phi());
    m_iter.clean();
    for (IR * opnd = xoc::iterInit(ir, m_iter);
         opnd != nullptr; opnd = xoc::iterNext(m_iter)) {
        if (!opnd->isReadPR()) { continue; }

        PRNO opndprno = opnd->getPrno();
        if (prset != nullptr && !prset->is_contain((BSIdx)opndprno)) {
            continue;
        }
        //Get the top-version on stack.
        Stack<VPR*> * vs = mapPRNO2VPRStack(opndprno);
        ASSERT0(vs);
        VPR * topv = vs->get_top();
        if (topv == nullptr) {
            //prno has no top-version, it has no def, may be parameter.
            ASSERT0(opnd->getSSAInfo());
            ASSERTN(VPR_version((VPR*)opnd->getSSAInfo()) == PRSSA_INIT_VERSION,
                    ("parameter only has first version"));
            VPR * zerov = getVPR(opndprno, PRSSA_INIT_VERSION);
            ASSERT0(zerov == opnd->getSSAInfo());
            zerov->addUse(opnd);
            continue;
        }

        //e.g: pr1 = pr2(vp1)
        //    vp1 will be renamed to vp2, so vp1 does not
        //    occur in current IR any more.
        VPR * curv = (VPR*)opnd->getSSAInfo();
        ASSERT0(curv && curv->orgprno() == opndprno);

        //Let latest version VPR regard current opnd as an USE.
        if (VPR_version(topv) == PRSSA_INIT_VERSION) {
            //Each opnd only meet once.
            ASSERT0(curv == topv);
            ASSERT0(!topv->findUse(opnd));
            topv->addUse(opnd);
            continue;
        }

        if (curv != topv) {
            //curv may be ver0.
            //Current ir does not refer the old version VPR any more.
            ASSERT0(VPR_version(curv) == PRSSA_INIT_VERSION ||
                    curv->findUse(opnd));
            ASSERT0(VPR_version(curv) == PRSSA_INIT_VERSION ||
                    SSA_def(curv) != nullptr);
            ASSERT0(!topv->findUse(opnd));

            curv->removeUse(opnd);
            opnd->setSSAInfo(topv);
            topv->addUse(opnd);
        }
    }
}


//Remove PR-SSA Use-Def chain for all memory references in IR Tree
//that rooted by 'exp'.
//e.g:ir=...
//    ...=ir //S1
//If S1 deleted, ir should be removed from its UseSet in SSAInfo.
//NOTE: If ir is an IR tree, e.g: add(pr1, pr2), removing 'add' means
//pr1 and pr2 will be removed as well. Therefore pr1 pr2's SSAInfo will be
//updated as well.
void PRSSAMgr::removePRSSAOcc(IR const* ir)
{
    if (ir->is_stmt()) {
        if (ir->isWritePR() || ir->isCallStmt()) {
            SSAInfo * prssainfo = ir->getSSAInfo();
            if (prssainfo != nullptr) {
                prssainfo->cleanDU();
            }
        }
    } else if (ir->isPROp()) {
        SSAInfo * prssainfo = ir->getSSAInfo();
        if (prssainfo != nullptr) {
            prssainfo->removeUse(ir);
        }
    }
    //Whole IR tree may be removed via this function recursively.
    //Maintain the SSAInfo of read-pr/write-pr operation.
    for (UINT i = 0; i < IR_MAX_KID_NUM(ir); i++) {
        for (IR * x = ir->getKid(i); x != nullptr; x = x->get_next()) {
            removePRSSAOcc(x);
        }
    }
}


//Remove Use-Def chain.
//exp: the expression to be removed.
//e.g: ir = ...
//    = ir //S1
//If S1 will be deleted, ir should be removed from its useset in MDSSAInfo.
//NOTE: the function only process exp itself.
void PRSSAMgr::removeUse(IR const* ir)
{
    SSAInfo * info = ir->getSSAInfo();
    if (info != nullptr) {
        info->removeUse(ir);
    }
}


//Remove Use-Def chain.
//exp: the expression to be removed.
//e.g: ir = ...
//    = ir //S1
//If S1 will be deleted, ir should be removed from its useset in MDSSAInfo.
//NOTE: the function only process exp itself.
void PRSSAMgr::removeUseForTree(IR const* ir)
{
    ConstIRIter it;
    for (IR const* x = iterInitC(ir, it, false);
         x != nullptr; x = iterNextC(it, true)) {
        if (x->isPROp()) {
            PRSSAMgr::removeUse(x);
        }
    }
}


void PRSSAMgr::removeDUChain(IR * def, IR * use)
{
    ASSERT0(def && def->is_stmt() && def->isPROp());
    ASSERT0(use && use->is_exp() && use->isPROp());
    def->getSSAInfo()->removeUse(use);
    use->cleanSSAInfo();
}


//Check each USE of stmt, remove the expired one which is not reference
//the memory any more that stmt defined.
bool PRSSAMgr::removeExpiredDU(IR const* ir, Region * rg)
{
    ASSERT0(ir && ir->isPROp());
    SSAInfo * ssainfo = ir->getSSAInfo();
    if (ssainfo == nullptr || SSA_def(ssainfo) == nullptr) { return false; }

    PRNO prno = SSA_def(ssainfo)->getPrno();

    //Check if use referenced the PR which defined by SSA_def.
    BSIdx ni = BS_UNDEF;
    bool change = false;
    SSAUseIter si;
    for (BSIdx i = SSA_uses(ssainfo).get_first(&si); si != nullptr; i = ni) {
        ni = SSA_uses(ssainfo).get_next(i, &si);
        IR const* use = rg->getIR(i);
        if (use->is_pr() && PR_no(use) == prno) { continue; }

        ssainfo->removeUse(use);
        change = true;
    }
    return change;
}


//Check each USE of stmt, remove the expired one which is not reference
//the memory any more that stmt defined.
bool PRSSAMgr::removeExpiredDUForStmt(IR const* stmt, Region * rg)
{
    ASSERT0(stmt->is_stmt());
    SSAInfo * ssainfo = stmt->getSSAInfo();
    if (ssainfo == nullptr) { return false; }

    SSAUseIter si;
    PRNO prno = PRNO_UNDEF;
    if (SSA_def(ssainfo) != nullptr) {
        prno = SSA_def(ssainfo)->getPrno();
    } else {
        //Without DEF.
        return false;
    }

    //Check if use referenced the number of PR which defined by SSA_def.
    BSIdx ni = BS_UNDEF;
    bool change = false;
    for (BSIdx i = SSA_uses(ssainfo).get_first(&si); si != nullptr; i = ni) {
        ni = SSA_uses(ssainfo).get_next(i, &si);
        IR const* use = rg->getIR(i);
        if (use->is_pr() && PR_no(use) == prno) { continue; }

        ssainfo->removeUse(use);
        change = true;
    }
    return change;
}


//ir: may be Phi.
void PRSSAMgr::renameLHS(MOD IR * ir, PRSet const* prset)
{
    ASSERT0(ir->is_stmt());
    IR * res = ir->getResultPR();
    if (res == nullptr) { return; }
    PRNO resprno = res->getPrno();
    if (prset != nullptr && !prset->is_contain((BSIdx)resprno)) {
        return;
    }
    //Rename result, include phi.
    ASSERT0(res->is_single());
    VPR * resvp = (VPR*)res->getSSAInfo();
    ASSERT0(resvp);
    PRNO orgprno = resvp->orgprno();
    ASSERTN(orgprno == resprno, ("PRNO is unchanged until stripping version"));
    UINT maxv = m_prno2maxversion.get((VecIdx)orgprno);
    VPR * new_v = allocVPR(orgprno, maxv + 1, res->getType());
    m_prno2maxversion.set((VecIdx)orgprno, maxv + 1);
    mapPRNO2VPRStack(orgprno)->push(new_v);
    res->setSSAInfo(new_v);
    SSA_def(new_v) = ir;
    new_v->cleanUseSet();
}


//Rename VPR from current version to the top-version on stack if it exist.
void PRSSAMgr::renameBB(IRBB const* bb, PRSet const* prset)
{
    BBIRList & irlist = const_cast<IRBB*>(bb)->getIRList();
    BBIRListIter it;
    for (IR * ir = irlist.get_head(&it);
         ir != nullptr; ir = irlist.get_next(&it)) {
       if (!ir->is_phi()) {
           renameRHS(ir, prset);
       }
       renameLHS(ir, prset);
    }
}


Stack<VPR*> * PRSSAMgr::mapPRNO2VPRStack(PRNO prno)
{
    Stack<VPR*> * stack = m_prno2stack.get((VecIdx)prno);
    if (stack == nullptr) {
        stack = new Stack<VPR*>();
        m_prno2stack.set((VecIdx)prno, stack);
    }
    return stack;
}


//idx: index of operand, start from 0.
void PRSSAMgr::handlePhiOpndInSucc(IR * ir, UINT idx, PRSet const* prset)
{
    ASSERT0(ir->is_phi());
    //Update version for same PR.
    IR * opnd = PHI_opnd_list(ir);
    UINT j = 0;
    while (opnd != nullptr && j < idx) {
        opnd = opnd->get_next();
        j++;
    }
    ASSERT0(j == idx && opnd);
    if (!opnd->isPROp()) { return; }
    if (prset != nullptr && !prset->is_contain((BSIdx)opnd->getPrno())) {
        return;
    }
 
    VPR * old = (VPR*)PR_ssainfo(opnd);
    ASSERT0(old != nullptr);
    VPR * topv = mapPRNO2VPRStack(old->orgprno())->get_top();
    ASSERT0(topv != nullptr);

    old->removeUse(opnd);
    if (SSA_def(topv) != nullptr) {
        //topv might be zero version.
        IR * defres = SSA_def(topv)->getResultPR(topv->orgprno());
        ASSERT0(defres);

        IR * new_opnd = m_rg->getIRMgr()->buildPRdedicated(defres->getPrno(),
                                               defres->getType());
        new_opnd->copyRef(defres, m_rg);
        xcom::replace(&PHI_opnd_list(ir), opnd, new_opnd);
        IR_parent(new_opnd) = ir;
        m_rg->freeIRTree(opnd);
        opnd = new_opnd;

        //Phi should have same type with opnd.
        IR_dt(ir) = IR_dt(defres);
    }

    PR_ssainfo(opnd) = topv;

    //Add version0 opnd here, means opnd will be added to ver0
    //use-list if topv is version0. There is no need to
    //add version0 at placePhi.
    topv->addUse(opnd);
}


void PRSSAMgr::handleSuccOfBB(IRBB * bb, BBSet const* bbset,
                              PRSet const* prset)
{
    //Replace the jth opnd of PHI with 'topv' which in bb's successor.
    for (xcom::EdgeC const* ec = bb->getVex()->getOutList();
         ec != nullptr; ec = ec->get_next()) {
        xcom::Vertex const* succv = ec->getTo();
        if (bbset != nullptr && !bbset->is_contain(succv->id())) {
            continue;
        }
        IRBB const* succ = m_cfg->getBB(succv->id());
        ASSERT0(succ);
        //Compute which predecessor 'bb' is with respect to its successor.
        //the index of corresponding predecessor.
        UINT idx = xcom::Graph::WhichPred(bb->id(), succv);

        //Replace opnd of PHI of 'succ' with top SSA version.
        IRListIter ct;
        for (IR * ir = BB_irlist(succ).get_head(&ct);
             ir != nullptr; ir = BB_irlist(succ).get_next(&ct)) {
            if (!ir->is_phi()) {
                break;
            }
            handlePhiOpndInSucc(ir, idx, prset);
        }
    }
}


//The function rename PR in BB.
//defined_prs: record the PR set that defined in 'bb' if exist.
void PRSSAMgr::handleBBRename(PRSet const* defined_prs,
                              BBSet const* bbset, PRSet const* prset,
                              MOD IRBB * bb, MOD BB2VPMap & bb2vp)
{
    ASSERT0(bb2vp.get(bb->id()) == nullptr);
    PRNO2VPR * prno2vp = bb2vp.allocPRNO2VPR(bb->id());
    DefSBitSetIter cur = nullptr;
    if (defined_prs != nullptr) {
        for (BSIdx prno = defined_prs->get_first(&cur);
             prno != BS_UNDEF; prno = defined_prs->get_next(prno, &cur)) {
            VPR * vp = mapPRNO2VPRStack((PRNO)prno)->get_top();
            ASSERT0(vp);
            prno2vp->set(vp->orgprno(), vp);
        }
    }
    renameBB(bb, prset);

    //Rename PHI operand in which successor BB placed.
    handleSuccOfBB(bb, bbset, prset);
}


//Linear renaming algorithm.
//bb2definedprs: for each BB, indicate PRs which has been defined.
//bbset: if not null, indicates perform the renaming inside designated BB set.
void PRSSAMgr::renameInDomTreeOrder(MOD IRBB * root,
                                    DomTree const& domtree,
                                    BB2PRSet const& bb2definedprs,
                                    BBSet const* bbset,
                                    PRSet const* prset)
{
    ASSERTN(root, ("SSA region must have root BB"));
    Stack<IRBB*> stk;
    UINT n = m_cfg->getBBList()->get_elem_count();
    xcom::BitSet visited(n / BIT_PER_BYTE);
    BB2VPMap bb2vpmap(n);
    ASSERT0(bbset == nullptr || bbset->is_contain(root->id()));
    IRBB * v;
    stk.push(root);
    List<IR*> lst; //for tmp use.
    while ((v = stk.get_top()) != nullptr) {
        if (!visited.is_contain(v->id())) {
            visited.bunion(v->id());
            handleBBRename(bb2definedprs.get(v->id()), bbset, prset, v,
                           bb2vpmap);
        }

        xcom::Vertex const* bbv = domtree.getVertex(v->id());
        bool all_visited = true;
        for (xcom::EdgeC const* c = bbv->getOutList();
             c != nullptr; c = c->get_next()) {
            xcom::Vertex * dom_succ = c->getTo();
            if (dom_succ == bbv || visited.is_contain(VERTEX_id(dom_succ))) {
                continue;
            }
            if (bbset != nullptr && !bbset->is_contain(dom_succ->id())) {
                continue;
            }

            ASSERT0(m_cfg->getBB(VERTEX_id(dom_succ)));
            all_visited = false;
            stk.push(m_cfg->getBB(VERTEX_id(dom_succ)));
            break;
        }
        if (!all_visited) { continue; }

        stk.pop();

        //Do post-processing while all kids of BB has been processed.
        PRNO2VPR * prno2vp = bb2vpmap.get(v->id());
        ASSERT0(prno2vp);
        PRSet const* defined_prs = bb2definedprs.get(v->id());
        if (defined_prs != nullptr) {
            DefSBitSetIter cur = nullptr;
            for (BSIdx i = defined_prs->get_first(&cur);
                 i != BS_UNDEF; i = defined_prs->get_next(i, &cur)) {
                Stack<VPR*> * vs = mapPRNO2VPRStack((PRNO)i);
                ASSERT0(vs->get_bottom() != nullptr);
                VPR * vp = prno2vp->get(vs->get_top()->orgprno());
                while (vs->get_top() != vp) {
                    vs->pop();
                }
            }
        }

        //vpmap is useless from now on.
        bb2vpmap.destoryPRNO2VPR(v->id());
    }
    ASSERT0(bb2vpmap.verify());
}


void PRSSAMgr::collectPRAndInitVPRForList(IRList const& irlist,
                                          BBSet const& bbset,
                                          MOD DefMiscBitSetMgr & sm,
                                          OUT BB2PRSet & bb2definedprs,
                                          OUT PRSet & prset)
{
    IRListIter it;
    for (IR * ir = irlist.get_head(&it);
         ir != nullptr; ir = irlist.get_next(&it)) {
        ASSERT0(ir->isPROp());
        PRNO prno = ir->getPrno();
        prset.bunion((BSIdx)prno);
        initVPR(ir, &prset);
        if (ir->is_exp()) {
            ASSERTN(bbset.is_contain(ir->getStmt()->getBB()->id()),
                    ("ir does not belong to bbset"));
            continue;
        }
        ASSERT0(ir->is_stmt() && ir->getBB());
        UINT bbid = ir->getBB()->id();
        ASSERTN(bbset.is_contain(bbid), ("ir does not belong to bbset"));
        bb2definedprs.addDefBB(prno, bbid);
    }

    //Allocate PRSet for given BBs, because placePhi() will need it later.
    //placePhi may add new PHI into PRSet.
    bb2definedprs.genPRSet(bbset);
}


//Rename dedicated PR in 'prset' which in specific BB set.
//prset: record PRs that expect to rename.
//root: root BB for CFG region that is consisted of BB which is in 'bbset'.
//bbset: specific a region that is consist of BB in 'bbset' that expect to
//       rename.
bool PRSSAMgr::constructDesignatedRegion(PRSSARegion & ssarg,
                                         DomTree const& domtree)
{
    START_TIMER(t, "PRSSA: Rename Designated PRSet");
    //Do necessary initialization for PRSet renaming.
    cleanPRNO2MaxVersion();
    cleanPRNO2VPRStack();
    cleanPRNO2Type();

    START_TIMER(t2, "PRSSA: Build dominance frontier");
    DfMgr dfm;
    dfm.build((xcom::DGraph&)*m_cfg);
    END_TIMER(t2, "PRSSA: Build dominance frontier");
    if (dfm.hasHighDFDensityVertex((xcom::DGraph&)*m_cfg)) {
        return false;
    }
    if (m_is_pruned) {
        m_livemgr = (LivenessMgr*)m_rg->getPassMgr()->
            registerPass(PASS_LIVENESS_MGR);
        m_livemgr->perform(*ssarg.getOptCtx());
        ASSERT0(m_livemgr->is_valid());
    } else {
        m_livemgr = nullptr;
    }
    IRList & irlist = ssarg.getIRList();
    BBSet & bbset = ssarg.getBBSet();
    DefMiscBitSetMgr sm;
    PRSet prset(sm.getSegMgr());
    BB2PRSet bb2definedprs(&sm);
    collectPRAndInitVPRForList(irlist, bbset, sm, bb2definedprs, prset);
    placePhi(dfm, prset, bbset, bb2definedprs);

    //Initialize VPR stack for given Prno.
    initVPRStack(prset);
    ASSERT0(m_cfg->getEntry());
    renameInDomTreeOrder(ssarg.getRootBB(), domtree, bb2definedprs,
                         &bbset, &prset);

    //Clean version stack after renaming.
    cleanPRNO2VPRStack();

    //Clean the mapping after renaming, and recompute the map if ssa needs
    //reconstruct.
    cleanPRNO2Type();
    stripVersionForBBSet(bbset, &prset);
    if (m_livemgr != nullptr) {
        m_livemgr->clean();
    }
    END_TIMER(t, "PRSSA: Rename Designated PRSet");
    return true;
}


void PRSSAMgr::initVPRStack(PRSet const& prset)
{
    DefSBitSetIter cur = nullptr;
    for (BSIdx prno = prset.get_first(&cur);
         prno != BS_UNDEF; prno = prset.get_next(prno, &cur)) {
        VPR * vp = allocVPR((PRNO)prno, PRSSA_INIT_VERSION, m_tm->getAny());
        mapPRNO2VPRStack(prno)->push(vp);
    }
}


//Rename PR.
void PRSSAMgr::rename(PRSet const& effect_prs, BB2PRSet const& bb2definedprs,
                      DomTree const& domtree)
{
    START_TIMER(t, "PRSSA: Rename");
    if (m_cfg->getBBList()->get_elem_count() == 0) { return; }
    initVPRStack(effect_prs);
    ASSERT0(m_cfg->getEntry());
    renameInDomTreeOrder(m_cfg->getEntry(), domtree, bb2definedprs,
                         nullptr, nullptr);
    END_TIMER(t, "PRSSA: Rename");
}


void PRSSAMgr::destructBBSSAInfo(IRBB * bb, OptCtx const& oc)
{
    IRListIter ct;
    IRListIter next_ct;
    BB_irlist(bb).get_head(&next_ct);
    ct = next_ct;
    for (; ct != BB_irlist(bb).end(); ct = next_ct) {
        next_ct = BB_irlist(bb).get_next(next_ct);
        IR * ir = ct->val();
        if (!ir->is_phi()) { break; }

        stripPhi(ir, ct, oc);
        BB_irlist(bb).remove(ct);
        m_rg->freeIRTree(ir);
    }
}


void PRSSAMgr::destructionInDomTreeOrder(IRBB * root, DomTree & domtree,
                                         OptCtx const& oc)
{
    Stack<IRBB*> stk;
    UINT n = m_cfg->getBBList()->get_elem_count();
    xcom::BitSet visited(n / BIT_PER_BYTE);
    BB2VPMap bb2vp(n);
    IRBB * v;
    stk.push(root);
    while ((v = stk.get_top()) != nullptr) {
        if (!visited.is_contain(v->id())) {
            visited.bunion(v->id());
            destructBBSSAInfo(v, oc);
        }

        xcom::Vertex * bbv = domtree.getVertex(v->id());
        ASSERTN(bbv, ("dom tree is invalid."));

        xcom::EdgeC * c = bbv->getOutList();
        bool all_visited = true;
        while (c != nullptr) {
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
void PRSSAMgr::destruction(DomTree & domtree, OptCtx const& oc)
{
    START_TIMER(t, "PRSSA: destruction in dom tree order");
    BBList * bblst = m_cfg->getBBList();
    if (bblst->get_elem_count() == 0) { return; }
    ASSERT0(m_cfg->getEntry());
    destructionInDomTreeOrder(m_cfg->getEntry(), domtree, oc);
    cleanPRSSAInfo();
    set_valid(false);
    END_TIMER(t, "PRSSA: destruction in dom tree order");
}


void PRSSAMgr::insertCopy(IRBB * pred, IR * store_to_phicopy)
{
    ASSERT0(pred && store_to_phicopy->is_stpr());
    //In PHI node elimination to insert the copy in the predecessor block,
    //there is a check if last IR of BB is not a call then
    //place the copy there only.
    //However for call BB terminator, the copy will be placed at the start
    //of fallthrough BB.
    IR * plast = pred->getLastIR();
    if (plast != nullptr && plast->isCallStmt()) {
        IRBB * fallthrough = m_cfg->getFallThroughBB(pred);
        if (plast->is_terminate()) {
            //No need insert copy, predecessor will teriminate program.
            return;
        }
        //Fallthrough BB does not exist if the last ir is terminate.
        ASSERTN(fallthrough, ("invalid control flow graph."));
        if (BB_irlist(fallthrough).get_head() == nullptr ||
            !BB_irlist(fallthrough).get_head()->is_phi()) {
            BB_irlist(fallthrough).append_head(store_to_phicopy);
            return;
        }
        //Insert basic block to hold the copy.
        IRBB * newbb = m_rg->allocBB();
        m_cfg->getBBList()->insert_after(newbb, pred);
        m_cfg->addBB(newbb);
        m_cfg->insertVertexBetween(pred->id(), fallthrough->id(),
                                   newbb->id());
        //Then append the copy.
        BB_irlist(newbb).append_head(store_to_phicopy);
        return;
    }
    pred->getIRList().append_tail_ex(store_to_phicopy);
}


//Return true if inserting copy at the head of fallthrough BB
//of current BB's predessor.
//Note that do not free phi at this function, it will be freed
//by user.
void PRSSAMgr::stripPhi(IR * phi, IRListIter phict, OptCtx const& oc)
{
    IRBB * bb = phi->getBB();
    ASSERT0(bb);
    xcom::Vertex const* vex = bb->getVex();
    ASSERT0(vex);
    //Temprarory RP to hold the result of PHI.
    IR * phicopy = m_rg->getIRMgr()->buildPR(phi->getType());
    phicopy->setMustRef(m_rg->getMDMgr()->genMDForPR(PR_no(phicopy),
                                                     phicopy->getType()),
                        m_rg);
    phicopy->cleanMayRef();
    IR * opnd = PHI_opnd_list(phi);

    //opnd may be CONST, LDA, PR.
    //ASSERT0(opnd->is_pr());
    ASSERT0(PHI_ssainfo(phi));

    UINT pos = 0;
    for (xcom::EdgeC * el = vex->getInList(), * nextel = nullptr;
         el != nullptr; el = nextel, opnd = opnd->get_next(), pos++) {
        ASSERT0(find_position(vex->getInList(), el) == pos);
        nextel = el->get_next();
        INT pred = el->getFromId();

        ASSERT0(opnd && opnd->is_exp());
        IR * opndcopy = m_rg->dupIRTree(opnd);
        if (!opndcopy->is_const()) {
            opndcopy->copyRefForTree(opnd, m_rg);
        }

        //The copy will be inserted into related predecessor.
        IR * store_to_phicopy = m_rg->getIRMgr()->buildStorePR(PR_no(phicopy),
                                                   phicopy->getType(),
                                                   opndcopy);
        store_to_phicopy->copyRef(phicopy, m_rg);
        insertCopy(m_cfg->getBB(pred), store_to_phicopy);
        //Remove the SSA DU chain between opnd and its DEF stmt.
        if (!opnd->isReadPR()) {
            //opnd may be constant.
            continue;
        }
        ASSERT0(PR_ssainfo(opnd));
        PR_ssainfo(opnd)->removeUse(opnd);
        if (haveToMaintainClassicPRDU(oc)) {
            xoc::removeUse(opnd, m_rg);
        }
    }

    IR * substitue_phi = m_rg->getIRMgr()->buildStorePR(PHI_prno(phi), phi->getType(),
                                            phicopy);
    substitue_phi->copyRef(phi, m_rg);
    BB_irlist(bb).insert_before(substitue_phi, phict);
    PHI_ssainfo(phi) = nullptr;
}


void PRSSAMgr::removePhiList()
{
    BBList * bblst = m_rg->getBBList();
    BBListIter bbit = nullptr;
    for (bblst->get_head(&bbit); bbit != nullptr;
         bbit = bblst->get_next(bbit)) {
        IRBB * bb = bbit->val();
        IRListIter irit = nullptr;
        IRListIter next_irit = nullptr;
        for (BB_irlist(bb).get_head(&irit); irit != nullptr; irit = next_irit) {
            next_irit = BB_irlist(bb).get_next(irit);
            IR * ir = irit->val();
            if (!ir->is_phi()) {
                //There is no phi any more.
                break;
            }
            PHI_ssainfo(ir) = nullptr;
            BB_irlist(bb).remove(irit);
            m_rg->freeIRTree(ir);
        }
    }
}


void PRSSAMgr::verifyPhiResult(IR const* ir, List<IRBB*> const& preds,
                               bool is_vpinfo_avail,
                               bool before_strip_version) const
{
    ASSERT0(ir->is_phi());
    //Check phi result.
    VPR * resvp = (VPR*)PHI_ssainfo(ir);
    if (is_vpinfo_avail) {
        ASSERTN(before_strip_version ?
                    resvp->orgprno() : resvp->newprno() == PHI_prno(ir),
                ("prno of VPR is unmatched"));
    }

    //Check the number of phi opnds.
    UINT num_opnd = 0;
    for (IR const* opnd = PHI_opnd_list(ir);
         opnd != nullptr; opnd = opnd->get_next()) {
        //Opnd may be PR, CONST or LDA.
        #ifdef _DEBUG_
        //Only for verification.
        if (is_vpinfo_avail) {
            PRNO prno = before_strip_version ?
                VPR_orgprno(PR_ssainfo(opnd)) :
                VPR_newprno(PR_ssainfo(opnd));
            ASSERTN(prno == PR_no(opnd),
                    ("prno of VPR is unmatched"));
        }
        #endif

        //Version0 is input parameter, and it has no SSA_def.
        //ASSERT0(VPR_version(PR_ssainfo(opnd)) > 0);

        num_opnd++;
    }

    ASSERTN(num_opnd == preds.get_elem_count(),
            ("The number of phi operand must same with "
             "the number of BB predecessors."));

    //Check SSA uses.
    SSAUseIter vit = nullptr;
    for (BSIdx i = SSA_uses(resvp).get_first(&vit);
         vit != nullptr; i = SSA_uses(resvp).get_next(i, &vit)) {
        IR * use = m_rg->getIR(i);
        ASSERT0(use->is_pr());
        ASSERTN(PR_no(use) == PHI_prno(ir), ("prno is unmatch"));
        SSAInfo * use_ssainfo = PR_ssainfo(use);
        ASSERT0_DUMMYUSE(use_ssainfo);
        ASSERT0(SSA_def(use_ssainfo) == ir);
    }
}


//This function verify def/use information of PHI stmt.
//If vpinfo is available, the function also check VPR_prno of phi operands.
//is_vpinfo_avail: set true if VPR information is available.
//before_strip_version: true if this function invoked before striping version.
bool PRSSAMgr::verifyPhi(bool is_vpinfo_avail, bool before_strip_version) const
{
    DUMMYUSE(is_vpinfo_avail);
    BBList * bblst = m_cfg->getBBList();
    List<IRBB*> preds;
    for (IRBB * bb = bblst->get_head(); bb != nullptr; bb = bblst->get_next()) {
        m_cfg->get_preds(preds, bb);
        IRListIter ct;
        for (BB_irlist(bb).get_head(&ct); ct != BB_irlist(bb).end();
             ct = BB_irlist(bb).get_next(ct)) {
            IR const* ir = ct->val();
            if (!ir->is_phi()) { break; }
            verifyPhiResult(ir, preds, is_vpinfo_avail, before_strip_version);
        }
    }
    return true;
}


//Check the consistency for IR_PR if VPR_newprno == PRNO.
//This function only can be invoked immediately
//after rename() and before refinePhi(), because refinePhi() might
//clobber VPR information, that leads VPR_orgprno() to be invalid.
bool PRSSAMgr::verifyPrnoOfVPR() const
{
    ConstIRIter ii;
    BBList const* bblst = m_cfg->getBBList();
    BBListIter ct;
    for (IRBB * bb = bblst->get_head(&ct);
         bb != nullptr; bb = bblst->get_next(&ct)) {
         for (IR * ir = BB_first_ir(bb); ir != nullptr; ir = BB_next_ir(bb)) {
            ii.clean();
            for (IR const* opnd = iterInitC(ir, ii);
                 opnd != nullptr; opnd = iterNextC(ii)) {
                if (opnd->is_pr()) {
                    ASSERT0(PR_no(opnd) == VPR_orgprno(PR_ssainfo(opnd)));
                }
            }
         }
     }
    return true;
}


//Verify VPR after striping version.
bool PRSSAMgr::verifyVPR() const
{
    //Check version for each vp.
    xcom::BitSet defset;
    for (VecIdx i = 1; i <= m_vpr_vec.get_last_idx(); i++) {
        VPR * v = m_vpr_vec.get(i);
        ASSERT0(v != nullptr);
        IR const* def = v->getDef();
        if (def == nullptr) {
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

        IR const* respr = nullptr;
        PRNO defprno = PRNO_UNDEF;
        if (def != nullptr) {
            respr = const_cast<IR*>(def)->getResultPR(v->newprno());
            ASSERTN(respr, ("Expect stmt result PR to be PR%d", v->newprno()));

            defprno = respr->getPrno();
            ASSERTN(defprno != PRNO_UNDEF, ("invalid PR built"));
        }

        SSAUseIter vit = nullptr;
        PRNO opndprno = PRNO_UNDEF;
        for (BSIdx i2 = SSA_uses(v).get_first(&vit);
             vit != nullptr; i2 = SSA_uses(v).get_next(i2, &vit)) {
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


static void verify_dominance(IR const* def, IR const* use, IRCFG * cfg)
{
    ASSERT0(def && use);
    if (use->getStmt()->is_phi()) {
        //If stmt's USE is operand of PHI, that may NOT be
        //dominated by stmt.
        return;
    }
    if (def->is_phi()) {
        //PHI may NOT always dominate its USEs.
        return;
    }
    IRBB const* defbb = def->getBB();
    IRBB const* usebb = use->getStmt()->getBB();
    ASSERT0(defbb == usebb || cfg->is_dom(defbb->id(), usebb->id()));
}


static void verify_use(IR * ir, SSAInfo * ssainfo, Region * rg,
                       OUT PRNO & opndprno)
{
    if (ir->is_exp()) {
        ASSERT0(ssainfo->findUse(ir));
    }
    SSAUseIter vit = nullptr;
    IRCFG * cfg = rg->getCFG();
    IR const* def = ssainfo->getDef();
    DomSet const* defdomset = nullptr;
    if (def != nullptr) {
        defdomset = cfg->get_dom_set(def->getBB()->id());
    }
    for (BSIdx i = SSA_uses(ssainfo).get_first(&vit);
         vit != nullptr; i = SSA_uses(ssainfo).get_next(i, &vit)) {
        IR * use = rg->getIR(i);
        ASSERT0(use->isPROp() || use->is_const() || use->is_lda());
        if (!use->isPROp()) { continue; }
        if (opndprno == PRNO_UNDEF) {
            opndprno = PR_no(use);
        } else {
            //All opnd should have same PR no.
            ASSERT0(opndprno == PR_no(use));
        }
        //Each USE of current SSAInfo must be defined by same stmt.
        ASSERT0(PR_ssainfo(use) == ssainfo);

        //DEF should dominate all USEs.
        if (def != nullptr && defdomset != nullptr) {
            verify_dominance(def, use, rg->getCFG());
        }
    }
}


static void verify_def(IR * ir, xcom::BitSet & defset, SSAInfo * ssainfo,
                       Region * rg, OUT PRNO & defprno)
{
    ASSERTN(ssainfo, ("%s miss SSA info.", IRNAME(ir)));
    IR * def = SSA_def(ssainfo);
    if (ir->is_stmt()) {
        ASSERTN(def == ir, ("ir does not have SSA du"));
        ASSERTN(!defset.is_contain(ir->id()),
                ("DEF for each PR must be unique."));
        defset.bunion(def->id());
    }

    IR const* respr = nullptr;
    if (def != nullptr) {
        ASSERT0(def->is_stmt());
        respr = def->getResultPR();
        ASSERT0(respr);
        defprno = respr->getPrno();
        ASSERT0(defprno != PRNO_UNDEF);
    }
}


static void verify_ssainfo_helper(IR * ir, xcom::BitSet & defset, Region * rg)
{
    ASSERT0(ir);
    SSAInfo * ssainfo = ir->getSSAInfo();
    PRNO defprno = PRNO_UNDEF;
    verify_def(ir, defset, ssainfo, rg, defprno);
    PRNO opndprno = PRNO_UNDEF;
    verify_use(ir, ssainfo, rg, opndprno);
    if (opndprno != PRNO_UNDEF && defprno != PRNO_UNDEF) {
        //Def should have same PR no with USE.
        ASSERT0(opndprno == defprno);
    }
}


//The verification check the DU info in SSA form.
//Current IR must be in SSA form.
bool PRSSAMgr::verifySSAInfo() const
{
    PRSSAMgr * pthis = const_cast<PRSSAMgr*>(this);
    //Check version for each vp.
    xcom::BitSet defset;
    BBList * bbl = m_cfg->getBBList();
    BBListIter ct;
    for (bbl->get_head(&ct); ct != bbl->end(); ct = bbl->get_next(ct)) {
        IRBB * bb = ct->val();
        IRListIter ctir;
        for (BB_irlist(bb).get_head(&ctir); ctir != BB_irlist(bb).end();
             ctir = BB_irlist(bb).get_next(ctir)) {
            IR * ir = ctir->val();
            pthis->m_iter.clean();
            for (IR * x = xoc::iterInit(ir, pthis->m_iter);
                 x != nullptr; x = xoc::iterNext(pthis->m_iter)) {
                if (x->isPROp()) {
                    verify_ssainfo_helper(x, defset, m_rg);
                }
            }
        }
    }
    return true;
}


//This function perform SSA destruction via scanning BB in sequential order.
//Note PRSSA will change PR no during PRSSA destruction. If classic DU chain
//is valid meanwhile, it might be disrupted as well. A better way is user
//maintain the classic DU chain, alternatively a conservative way to
//avoid subsequent verification complaining is set the prdu invalid.
void PRSSAMgr::destruction(MOD OptCtx & oc)
{
    if (!is_valid()) { return; }
    BBList * bblst = m_cfg->getBBList();
    if (bblst->get_elem_count() == 0) { return; }
    UINT bbcnt = bblst->get_elem_count();
    BBListIter bbct;
    for (bblst->get_head(&bbct);
         bbct != bblst->end(); bbct = bblst->get_next(bbct)) {
        IRBB * bb = bbct->val();
        ASSERT0(bb);
        destructBBSSAInfo(bb, oc);
    }
    if (bblst->get_elem_count() != bbcnt) {
        oc.setInvalidIfCFGChanged();
    }

    //Clean SSA info to avoid unnecessary abort or assert.
    cleanPRSSAInfo();
    set_valid(false);
}


//Set SSAInfo of IR to be nullptr to inform optimizer that IR is not in SSA form.
void PRSSAMgr::cleanPRSSAInfo()
{
    BBList * bblst = m_cfg->getBBList();
    BBListIter bbct = nullptr;
    for (bblst->get_head(&bbct);
         bbct != bblst->end(); bbct = bblst->get_next(bbct)) {
        IRBB * bb = bbct->val();
        ASSERT0(bb);
        IRListIter irct = nullptr;
        for (BB_irlist(bb).get_head(&irct);
             irct != BB_irlist(bb).end(); irct = BB_irlist(bb).get_next(irct)) {
            IR * ir = irct->val();
            ASSERT0(ir);
            m_iter.clean();
            for (IR * x = xoc::iterInit(ir, m_iter);
                 x != nullptr; x = xoc::iterNext(m_iter)) {
                ASSERTN(!x->is_phi(), ("phi should have been striped."));
                if (x->isPROp()) {
                    x->setSSAInfo(nullptr);
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

    SSAUseIter si = nullptr;
    BSIdx i = SSA_uses(irssainfo).get_first(&si);
    ASSERT0(si && i != BS_UNDEF);
    ASSERT0(rg->getIR(i)->is_pr());
    ASSERT0(PR_no(rg->getIR(i)) == PHI_prno(phi));

    IR_dt(phi) = rg->getIR(i)->getType();
}


static void iterCollectPhi(IR * phi, MOD List<IRBB*> & wl, MOD BitSet & in_list)
{
    //PHI is redundant, revise SSAInfo before removing the PHI.
    for (IR const* opnd = PHI_opnd_list(phi);
         opnd != nullptr; opnd = opnd->get_next()) {
        ASSERT0(opnd->isPhiOpnd());
        if (!opnd->is_pr()) {
            //CASE: phi=(10)
            //Note we leave the redundant case:phi=(10) to IR-Refinement
            //and CopyProp.
            continue;
        }
        SSAInfo * opndsi = PR_ssainfo(opnd);
        ASSERTN(opndsi, ("Miss SSAInfo."));
        if (SSA_def(opndsi) == nullptr || !SSA_def(opndsi)->is_phi()) {
            continue;
        }
        IRBB * defbb = SSA_def(opndsi)->getBB();
        ASSERTN(defbb, ("defbb does not belong to any BB"));
        wl.append_tail(defbb);
        if (!in_list.is_contain(defbb->id())) {
            wl.append_tail(defbb);
            in_list.bunion(defbb->id());
        }
    }
}


//common_def:record the DEF, it may be NULL if the common_def indicates
//           a region livein DEF,  or a constant expresssion.
static void replaceCommonDef(IR * phi, IR * common_def, IR * common_livein,
                             Region * rg)
{
    SSAInfo * def_or_livein_ssainfo = nullptr;
    PRNO def_or_livein_prno = PRNO_UNDEF;
    if (common_def != nullptr) {
        ASSERT0(common_livein == nullptr);
        IR * respr = common_def->getResultPR();
        ASSERT0(respr);
        def_or_livein_ssainfo = respr->getSSAInfo();
        def_or_livein_prno = respr->getPrno();
    } else {
        ASSERT0(common_livein && common_def == nullptr);
        def_or_livein_ssainfo = common_livein->getSSAInfo();
        def_or_livein_prno = common_livein->getPrno();
    }
    ASSERT0(def_or_livein_ssainfo && def_or_livein_prno != PRNO_UNDEF);

    SSAInfo * curphi_ssainfo = PHI_ssainfo(phi);
    ASSERT0(curphi_ssainfo);
    ASSERT0(SSA_def(curphi_ssainfo) == phi);
    if (phi != common_def) {
        //All operands of PHI are defined by same alternative stmt,
        //just call it common_def. Replace the SSA_def of
        //current SSAInfo to the common_def.
        SSA_uses(def_or_livein_ssainfo).bunion(SSA_uses(curphi_ssainfo));

        SSAUseIter it = nullptr;
        for (BSIdx i = SSA_uses(curphi_ssainfo).get_first(&it);
             it != nullptr; i = SSA_uses(curphi_ssainfo).get_next(i, &it)) {
            IR * use = rg->getIR(i);
            ASSERT0(use->is_pr());
            ASSERT0(PR_ssainfo(use) && PR_ssainfo(use) == curphi_ssainfo);
            PR_ssainfo(use) = def_or_livein_ssainfo;
            //Just change PRNO, keep type and other information unchanged.
            PR_no(use) = def_or_livein_prno;
        }
        for (IR const* opnd = PHI_opnd_list(phi);
             opnd != nullptr; opnd = opnd->get_next()) {
            ASSERT0(opnd->isPhiOpnd());
            if (!opnd->is_pr()) { continue; }
            SSAInfo * opndsi = PR_ssainfo(opnd);
            ASSERTN(opndsi, ("Miss SSAInfo."));
            opndsi->removeUse(opnd);
        }
    }
    ((VPR*)curphi_ssainfo)->cleanMember();
    curphi_ssainfo->cleanDU();
}


bool PRSSAMgr::refinePhiImpl(MOD IRBB * bb, MOD IR * ir,
                             MOD List<IRBB*> & wl, MOD BitSet & in_list,
                             IRListIter irct, OptCtx const& oc)
{
    IR * common_def = nullptr;
    IR * common_livein = nullptr;
    if (!isRedundantPHI(ir, &common_def, &common_livein)) {
        revisePhiDataType(ir, m_rg);
        return false;
    }
    if (common_def == nullptr && common_livein == nullptr) {
        //The redundant phi can not be optimized if no common-def found.
        return false;
    }
    iterCollectPhi(ir, wl, in_list);
    replaceCommonDef(ir, common_def, common_livein, m_rg);

    //Revise DU chains.
    //Note if SSA form is available, it still need to maintain
    //DU chain of PR in DU manager counterpart.
    xoc::removeStmt(ir, m_rg, oc);
    BB_irlist(bb).remove(irct);
    m_rg->freeIR(ir);
    return true;
}


//The function revises phi data type, and remove redundant phi.
//wl: work list for temporary used.
//Return true if there is phi removed.
bool PRSSAMgr::refinePhi(OptCtx const& oc)
{
    START_TIMER(t, "PRSSA: Refine phi");
    BBList * bblst = m_cfg->getBBList();
    BBListIter ct = nullptr;

    List<IRBB*> wl;
    //Estimate memory usage.
    BitSet in_list(bblst->get_elem_count() / BITS_PER_BYTE / 4);
    for (bblst->get_head(&ct); ct != bblst->end(); ct = bblst->get_next(ct)) {
        IRBB * bb = ct->val();
        ASSERT0(bb);
        IR const* first = BB_first_ir(bb);
        if (first != nullptr && first->is_phi()) {
            wl.append_tail(bb);
            in_list.bunion(bb->id());
        }
    }

    bool remove = false;
    IRBB * bb = nullptr;
    while ((bb = wl.remove_head()) != nullptr) {
        in_list.diff(bb->id());
        IRListIter irct = nullptr;
        IRListIter nextirct = nullptr;
        bool lremove = false;
        UINT oldcount = wl.get_elem_count();
        for (BB_irlist(bb).get_head(&nextirct), irct = nextirct;
             irct != BB_irlist(bb).end(); irct = nextirct) {
            nextirct = BB_irlist(bb).get_next(nextirct);
            IR * ir = irct->val();
            if (!ir->is_phi()) { break; }
            lremove |= refinePhiImpl(bb, ir, wl, in_list, irct, oc);
        }
        if (!lremove && wl.get_elem_count() > oldcount) {
            ASSERTN(0, ("Add more unoptimized phi"));
        }
        remove |= lremove;
    }
    END_TIMER(t, "PRSSA: Refine phi");
    return remove;
}


//Duplicate Phi operand that is at the given position, and insert after
//given position sequently.
//pos: given position
//num: the number of duplication.
//Note caller should guarrentee the number of operand is equal to the
//number predecessors of BB of Phi.
void PRSSAMgr::dupAndInsertPhiOpnd(IRBB const* bb, UINT pos, UINT num)
{
    ASSERT0(bb && num >= 1);
    BBIRListIter it;
    for (BB_irlist(bb).get_head(&it); it != nullptr;
         BB_irlist(bb).get_next(&it)) {
        IR * ir = it->val();
        if (!ir->is_phi()) { break; }
        ASSERT0(xcom::cnt_list(PHI_opnd_list(ir)) == bb->getNumOfPred());
        IR * opnd;
        UINT lpos = pos;
        for (opnd = PHI_opnd_list(ir); lpos != 0; opnd = opnd->get_next()) {
            ASSERT0(opnd);
            lpos--;
        }
        ASSERTN(opnd, ("PHI does not contain such many operands."));
        for (UINT i = 0; i < num; i++) {
            ((CPhi*)ir)->insertOpndAfter(opnd, m_rg->dupIR(opnd));
        }
    }
}


//Before removing BB or change BB successor.
//you need remove the related PHI operand if BB 'succ' has PHI.
void PRSSAMgr::removeSuccessorDesignatedPhiOpnd(IRBB const* succ, UINT pos)
{
    ASSERT0(succ);
    for (IR * ir = BB_first_ir(const_cast<IRBB*>(succ));
         ir != nullptr; ir = BB_next_ir(const_cast<IRBB*>(succ))) {
        if (!ir->is_phi()) { break; }

        //CASE:CFG optimization may have already remove the predecessor of
        //'succ' before call the function.
        //ASSERT0(xcom::cnt_list(PHI_opnd_list(ir)) == succ->getNumOfPred());

        IR * opnd;
        UINT lpos = pos;
        for (opnd = PHI_opnd_list(ir); lpos != 0; opnd = opnd->get_next()) {
            ASSERT0(opnd);
            lpos--;
        }

        if (opnd == nullptr) {
            //PHI does not contain any operand.
            continue;
        }

        opnd->removeSSAUse();
        ((CPhi*)ir)->removeOpnd(opnd);
        m_rg->freeIRTree(opnd);
    }
}


//This function revise phi data type, and remove redundant phi.
void PRSSAMgr::stripVersionForBBSet(BBSet const& bbset, PRSet const* prset)
{
    START_TIMER(t, "PRSSA: Strip PR version for BBSet");
    xcom::BitSet visited;

    //Ensure the first allocation of bitset could
    //accommodata the last vp id.
    visited.alloc(m_vp_count / BITS_PER_BYTE + 1);

    //Why not strip all VPR at once by iterating m_vpr_vec?
    //Just like:
    //  for (VecIdx i = 1; i <= m_vpr_vec.get_last_idx(); i++) {
    //    VPR * v = m_vpr_vec.get(i);
    //    ASSERT0(v != nullptr);
    //    stripSpecificVPR(v);
    //  }
    //Because the information of VPR during striping will not be maintained
    //and the relationship between VPR_orgprno and the concrete occurrence PR
    //may be invalid and that making the process assert.

    BBSetIter it = nullptr;
    for (BSIdx i = bbset.get_first(&it);
         i != BS_UNDEF; i = bbset.get_next(i, &it)) {
        IRBB * bb = m_cfg->getBB(i);
        ASSERT0(bb);
        IRListIter irct = nullptr;
        IRListIter nextirct = nullptr;
        for (BB_irlist(bb).get_head(&nextirct), irct = nextirct;
             irct != BB_irlist(bb).end(); irct = nextirct) {
            nextirct = BB_irlist(bb).get_next(nextirct);
            stripStmtVersion(irct->val(), prset, visited);
        }
    }

    END_TIMER(t, "PRSSA: Strip PR version for BBSet");
}


//This function revise phi data type, and remove redundant phi.
void PRSSAMgr::stripVersionForBBList(BBList const& bblst)
{
    START_TIMER(t, "PRSSA: Strip PR version for BBList");
    if (bblst.get_elem_count() == 0) { return; }

    BBListIter ct = nullptr;
    xcom::BitSet visited;

    //Ensure the first allocation of bitset could
    //accommodata the last vp id.
    visited.alloc(m_vp_count / BITS_PER_BYTE + 1);

    //Why not strip all VPR at once by iterating m_vpr_vec?
    //Just like:
    //  for (VecIdx i = 1; i <= m_vpr_vec.get_last_idx(); i++) {
    //    VPR * v = m_vpr_vec.get(i);
    //    ASSERT0(v != nullptr);
    //    stripSpecificVPR(v);
    //  }
    //Because the information of VPR during striping will not be maintained
    //and the relationship between VPR_orgprno and the concrete occurrence PR
    //may be invalid and that making the process assert.

    for (bblst.get_head(&ct); ct != bblst.end(); ct = bblst.get_next(ct)) {
        IRBB * bb = ct->val();
        IRListIter irct = nullptr;
        IRListIter nextirct = nullptr;
        for (BB_irlist(bb).get_head(&nextirct), irct = nextirct;
             irct != BB_irlist(bb).end(); irct = nextirct) {
            nextirct = BB_irlist(bb).get_next(nextirct);
            stripStmtVersion(irct->val(), nullptr, visited);
        }
    }

    END_TIMER(t, "PRSSA: Strip PR version for BBList");
}


//Return the replaced one.
static IR * replaceResultPR(IR * stmt, PRNO oldprno, PRNO newprno,
                            Type const* newprty)
{
    DUMMYUSE(oldprno);
    //newprty may be ANY.
    ASSERT0(newprno != PRNO_UNDEF);

    //Replace stmt PR and DATA_TYPE info.
    ASSERT0(stmt->getPrno() == oldprno);
    stmt->setPrno(newprno);
    IR_dt(stmt) = newprty;
    return stmt;
}


//Strip specified VPR's version.
void PRSSAMgr::stripSpecificVPR(VPR * vp)
{
    IR * def = vp->getDef();
    PRNO orgprno = vp->orgprno();
    if (def == nullptr) {
        ASSERT0(VPR_version(vp) == PRSSA_INIT_VERSION);
        setVPR(orgprno, vp);
        return;
    }

    ASSERT0(VPR_version(vp) != PRSSA_INIT_VERSION);
    ASSERTN(getVPRByPRNO(orgprno) == nullptr ||
            getVPRByPRNO(orgprno)->version() == PRSSA_INIT_VERSION,
            ("the mapping only available for newprno"));
    ASSERTN(def->getResultPR(orgprno), ("Stmt result must be PR%d", orgprno));
    Type const* newprty = def->getResultPR(orgprno)->getType();
    PRNO newprno = m_rg->getIRMgr()->buildPrno(newprty);
    IR * replaced_one = replaceResultPR(def, orgprno, newprno, newprty);
    ASSERT0(replaced_one);

    MD const* md = m_rg->getMDMgr()->genMDForPR(newprno, newprty);
    replaced_one->setMustRef(md, m_rg);
    SSAUseIter vit = nullptr;
    for (BSIdx i = SSA_uses(vp).get_first(&vit);
         vit != nullptr; i = SSA_uses(vp).get_next(i, &vit)) {
        IR * use = m_rg->getIR(i);
        ASSERT0(use->is_pr());

        //Rename PR.
        PR_no(use) = newprno;

        //Keep the data type of reference unchanged.
        //IR_dt(use) = newprty;

        //Update MD reference to new PR.
        use->setMustRef(md, m_rg);
    }

    //Record the newprno to avoid verifyVPR() assertion.
    //However, checking VPR_orgprno after strip_version is dispensable.
    //Original prno will useful when PHI operand update incrementally.
    VPR_newprno(vp) = newprno;
    setVPR(newprno, vp);
}


void PRSSAMgr::findAndSetLiveinDefForTree(IR * exp)
{
    IRIter it;
    for (IR * x = iterInit(exp, it); x != nullptr; x = iterNext(it)) {
        if (x->isReadPR()) {
            findAndSetLiveinDef(x);
        }
    }
}


void PRSSAMgr::findAndSetLiveinDef(IR * exp)
{
    ASSERT0(exp->isReadPR());
    VPR * vpr = getVPRByPRNO(exp->getPrno());
    ASSERT0(vpr);
    ASSERT0(exp->getSSAInfo() == nullptr || exp->getSSAInfo() == vpr);
    PR_ssainfo(exp) = vpr;
    vpr->addUse(exp);
}


void PRSSAMgr::stripStmtVersion(IR * stmt, PRSet const* prset,
                                xcom::BitSet & visited)
{
    ASSERT0(stmt->is_stmt());
    if (stmt->getResultPR() != nullptr &&
        (prset == nullptr || prset->is_contain((BSIdx)stmt->getPrno()))) {
        VPR * vp = (VPR*)stmt->getSSAInfo();
        ASSERT0(vp);
        if (!visited.is_contain(vp->id())) {
            ASSERT0(vp->version() != PRSSA_INIT_VERSION);
            //Avoid restriping again.
            visited.bunion(vp->id());
            stripSpecificVPR(vp);
        }
    }

    //Process operand.
    m_iter.clean();
    for (IR const* k = xoc::iterExpInit(stmt, m_iter);
         k != nullptr; k = xoc::iterExpNext(m_iter)) {
        if (!k->isReadPR()) { continue; }
        if (prset != nullptr && !prset->is_contain((BSIdx)k->getPrno())) {
            continue;
        }
        VPR * vp = (VPR*)k->getSSAInfo();
        ASSERT0(vp);
        if (!visited.is_contain(vp->id())) {
            //Version may be zero if there is not any DEF for k.
            //ASSERT0(VPR_version(vp) != PRSSA_INIT_VERSION);

            //Avoid restriping again.
            visited.bunion(vp->id());

            stripSpecificVPR(vp);
        }
    }
}


//Construct DU chain which need by DUMgr.
//This function will build the DUSet for PHI and its USE.
void PRSSAMgr::constructMDDUChainForPR()
{
    for (VecIdx i = 1; i <= m_vpr_vec.get_last_idx(); i++) {
        VPR * v = m_vpr_vec.get(i);
        ASSERT0(v != nullptr);
        IR * def = SSA_def(v);
        if (def == nullptr) { continue; }
        ASSERT0(def->is_stmt());
        SSAUseIter vit = nullptr;
        for (BSIdx i2 = SSA_uses(v).get_first(&vit);
             vit != nullptr; i2 = SSA_uses(v).get_next(i2, &vit)) {
            IR * use = m_rg->getIR(i2);
            ASSERT0(use->is_pr());
            ASSERT0(def->isExactDef(use->getRefMD()));
            m_rg->getDUMgr()->buildDUChain(def, use);
        }
    }
}


SSAInfo * PRSSAMgr::genSSAInfoForStmt(IR * stmt)
{
    ASSERT0(stmt->is_stmt() && stmt->isPROp());
    ASSERT0(stmt->getSSAInfo() == nullptr);

    //Check if same PR has been defined multiple times.
    ASSERTN(getSSAInfoByPRNO(stmt->getPrno()) == nullptr,
            ("found multi-definition, ir is not in SSA mode"));
    SSAInfo * ssainfo = allocSSAInfo(stmt->getPrno(), stmt->getType());
    ASSERTN(SSA_def(ssainfo) == nullptr,
            ("multidefinition in for PR%d", stmt->getPrno()));
    SSA_def(ssainfo) = stmt;
    stmt->setSSAInfo(ssainfo);
    return ssainfo;
}


SSAInfo * PRSSAMgr::genSSAInfoForExp(IR * exp)
{
    ASSERT0(exp->is_exp() && exp->isPROp());
    ASSERT0(exp->getSSAInfo() == nullptr);

    //Check if same PR has been defined multiple times.
    ASSERTN(getSSAInfoByPRNO(exp->getPrno()) == nullptr,
            ("found duplicated SSAInfo for same PRNO"));
    SSAInfo * ssainfo = allocSSAInfo(exp->getPrno(), exp->getType());
    ASSERT0(SSA_def(ssainfo) == nullptr);
    exp->setSSAInfo(ssainfo);
    ssainfo->addUse(exp);
    return ssainfo;
}


void PRSSAMgr::genSSAInfoForBBList()
{
    BBListIter bbct = nullptr;
    BBList * bblst = m_cfg->getBBList();
    for (bblst->get_head(&bbct); bbct != bblst->end(); bblst->get_next(&bbct)) {
        IRBB * bb = bbct->val();
        IRListIter irct = nullptr;
        for (BB_irlist(bb).get_head(&irct);
             irct != BB_irlist(bb).end();
             irct = BB_irlist(bb).get_next(irct)) {
            IR * ir = irct->val();
            if (!ir->isPROp()) { continue; }
            genSSAInfoForStmt(ir);
        }
    }
}


void PRSSAMgr::genSSAInfoForExp()
{
    IRIter ii;
    BBListIter bbct = nullptr;
    BBList * bblst = m_cfg->getBBList();
    for (bblst->get_head(&bbct); bbct != bblst->end(); bblst->get_next(&bbct)) {
        IRBB * bb = bbct->val();
        IRListIter irct = nullptr;
        for (BB_irlist(bb).get_head(&irct); irct != BB_irlist(bb).end();
             irct = BB_irlist(bb).get_next(irct)) {
            IR * ir = irct->val();
            ii.clean();
            for (IR * x = xoc::iterExpInit(ir, ii);
                 x != nullptr; x = xoc::iterExpNext(ii)) {
                if (!x->isPROp()) { continue; }
                ASSERT0(x->getSSAInfo() == nullptr);
                if (!x->isReadPR()) { continue; }
                SSAInfo * ssainfo = getSSAInfoByPRNO(x->getPrno());
                if (ssainfo == nullptr) {
                    ssainfo = allocSSAInfo(x->getPrno(), x->getType());
                } else if (ssainfo->getDef() != nullptr) {
                    ASSERT0(ssainfo->getDef()->isPROp() &&
                            ssainfo->getDef()->getPrno() == x->getPrno());
                } else {
                    //The PR is region livein.
                }
                ssainfo->addUseAndSSAInfo(x);
            }
        }
    }
}


//Compute SSAInfo for IRs in region that are in SSA mode.
//Note the function does NOT maintain Version info for PR.
void PRSSAMgr::genSSAInfoForRegion()
{
    reinit();
    genSSAInfoForBBList();
    genSSAInfoForExp();
    set_valid(true);
}


//Return true if stmt dominates use's stmt, otherwise return false.
bool PRSSAMgr::isStmtDomUseInsideLoop(IR const* stmt, IR const* use,
                                      LI<IRBB> const* li,
                                      OptCtx const& oc) const
{
    IRBB const* usestmtbb = nullptr;
    ASSERT0(use->getStmt());
    usestmtbb = use->getStmt()->getBB();
    ASSERT0(usestmtbb);

    if (!li->isInsideLoop(usestmtbb->id())) {
        //Only check dominiation info inside loop.
        return true;
    }

    IRBB const* defstmtbb = stmt->getBB();
    ASSERT0(defstmtbb);
    ASSERT0(oc.is_dom_valid());
    if (defstmtbb != usestmtbb &&
        m_cfg->is_dom(defstmtbb->id(), usestmtbb->id())) {
        return true;
    }
    if (defstmtbb == usestmtbb) {
        return defstmtbb->is_dom(stmt, use->getStmt(), true);
    }
    return false;
}


//Return true if ir dominates all its USE expressions which inside loop.
//In ssa mode, stmt's USE may be placed in operand list of PHI.
bool PRSSAMgr::isStmtDomAllUseInsideLoop(IR const* ir, LI<IRBB> const* li,
                                         OptCtx const& oc) const
{
    ASSERT0(ir);
    SSAInfo * info = ir->getSSAInfo();
    ASSERTN(info, ("miss PRSSAInfo"));
    ASSERT0(info->getDef() == ir);
    ASSERT0(ir->getBB());
    SSAUseIter iter;
    for (BSIdx i = info->getUses().get_first(&iter);
         iter != nullptr; i = info->getUses().get_next(i, &iter)) {
        IR const* use = const_cast<Region*>(m_rg)->getIR(i);
        if (!use->is_pr()) {
            ASSERT0(!use->isReadPR());
            continue;
        }
        ASSERTN(PR_no(use) == ir->getPrno(), ("prno is unmatch"));
        ASSERT0(PR_ssainfo(use) == info);
        if (!isStmtDomUseInsideLoop(ir, use, li, oc)) {
            return false;
        }
    }
    return true;
}


//Move IR_PHI from 'from' to 'to'.
//This function often used in updating PHI when adding new dominater
//BB to 'to'.
void PRSSAMgr::movePhi(IRBB * from, IRBB * to)
{
    IRListIter irct_to = nullptr;
    IRListIter irct_from = nullptr;
    IRListIter irct_from_next = nullptr;
    BBIRList & toirlist = to->getIRList();
    BBIRList & fromirlist = from->getIRList();
    toirlist.get_head(&irct_to);
    for (fromirlist.get_head(&irct_from); irct_from != fromirlist.end();
         irct_from = irct_from_next) {
        IR * ir = irct_from->val();
        if (!ir->is_phi()) { break; }

        irct_from_next = fromirlist.get_next(irct_from);
        //Move ir from 'from' to 'to'.
        fromirlist.remove(irct_from);
        if (irct_to == nullptr) {
            //to is empty BB.
            irct_to = toirlist.append_head(ir);
        } else {
            //Make sure phi's order in 'to' is same with 'from'.
            irct_to = toirlist.insert_before(ir, irct_to);
        }
    }
}


bool PRSSAMgr::verifyPRSSAInfo(Region const* rg)
{
    PRSSAMgr const* ssamgr = rg->getPRSSAMgr();
    if (ssamgr != nullptr && ssamgr->is_valid()) {
        ASSERT0(ssamgr->verifySSAInfo());
        ASSERT0(ssamgr->verifyPhi(false, false));
        //TBD:Do we have to verify VPR here?
        //ASSERT0(ssamgr->verifyVPR());
    }
    return true;
}


static void iterPhiToGenLab(IRBB const* bb, IRCFG const* cfg,
                            MOD List<IRBB*> & preds)
{
    preds.clean();
    IRCFG * pcfg = const_cast<IRCFG*>(cfg);
    IRBB * pbb = const_cast<IRBB*>(bb);
    pcfg->get_preds(preds, bb);
    for (IR * ir = pbb->getIRList().get_head();
         ir != nullptr; ir = pbb->getIRList().get_next()) {
        if (!ir->is_phi()) { break; }
        ASSERT0(preds.get_elem_count() ==
                xcom::cnt_list(PHI_opnd_list(ir)));
        BBListIter predbbct = nullptr;
        preds.get_head(&predbbct);
        for (IR * opnd = PHI_opnd_list(ir); opnd != nullptr;
             opnd = opnd->get_next(), predbbct = preds.get_next(predbbct)) {
            xcom::C<LabelInfo const*> * lct = nullptr;
            LabelInfo const* lab = predbbct->val()->getLabelListConst().
                                   get_head(&lct);
            if (lab != nullptr) { continue; }
            //Add label to BB because each opnd of PHI has to correspond to
            //an unique label.
            lab = cfg->getRegion()->genILabel();
            pcfg->addLabel(predbbct->val(), lab);
        }
        break;
    }
}


//Generate Label for the predecessor BB that corresponding to the specific
//phi operand.
void PRSSAMgr::genLabForInputBBOfPhiOpnd(IRCFG const* cfg)
{
    ASSERT0(cfg);
    List<IRBB*> preds;
    BBListIter bbct = nullptr;
    BBList const* bblist = const_cast<IRCFG*>(cfg)->getBBList();
    for (bblist->get_head(&bbct);
         bbct != bblist->end(); bbct = bblist->get_next(bbct)) {
        iterPhiToGenLab(bbct->val(), cfg, preds);
    }
}


//Return true if the value of ir1 and ir2 are definitely same, otherwise
//return false to indicate unknown.
bool PRSSAMgr::hasSameValue(IR const* ir1, IR const* ir2)
{
    ASSERT0(ir1->isPROp() && ir2->isPROp());
    SSAInfo const* info1 = ir1->getSSAInfo();
    SSAInfo const* info2 = ir2->getSSAInfo();
    ASSERT0(info1 && info2);
    return info1 == info2;
}


void PRSSAMgr::changeDef(IR * olddef, IR * newdef)
{
    ASSERT0(olddef && newdef && olddef->isPROp() && newdef->isPROp());
    ASSERT0(olddef->is_stmt() && newdef->is_stmt());
    ASSERT0(olddef != newdef);
    SSAInfo * oldssainfo = olddef->getSSAInfo();
    ASSERT0(oldssainfo);
    SSAInfo * newssainfo = newdef->getSSAInfo();
    if (newssainfo == nullptr) {
        newssainfo = allocSSAInfo(newdef->getPrno(), newdef->getType());
        newdef->setSSAInfo(newssainfo);
        SSA_def(newssainfo) = newdef;
    }
    PRNO newprno = newdef->getPrno();
    SSAUseIter itold;
    for (BSIdx i = oldssainfo->getUses().get_first(&itold);
         itold != nullptr; i = oldssainfo->getUses().get_next(i, &itold)) {
        IR * use = m_rg->getIR(i);
        if (!use->isPROp()) { continue; }
        ASSERTN(use->getPrno() == olddef->getPrno(), ("prno is unmatch"));
        ASSERT0(use->getSSAInfo() == oldssainfo);
        use->setPrno(newprno);
        use->setSSAInfo(newssainfo);
        use->copyRef(newdef, m_rg);
    }
    newssainfo->copyUseSet(*oldssainfo);
    oldssainfo->cleanUseSet();
}


//The function revise classic PRDU if PRSSA constructed.
static void removeClassicPRDUForCallStmt(Region * rg)
{
    DUMgr * dumgr = rg->getDUMgr();
    if (dumgr == nullptr) { return; }
    BBList const* bblist = rg->getBBList();
    BBListIter bbit;
    for (IRBB * bb = bblist->get_head(&bbit);
         bb != nullptr; bb = bblist->get_next(&bbit)) {
        BBIRListIter irit;
        for (IR const* ir = bb->getIRList().get_head(&irit);
             ir != nullptr; ir = bb->getIRList().get_next(&irit)) {
            if (ir->isCallStmt()) {
                dumgr->removePRFromDUSet(ir);
            }
        }
    }
}


void PRSSAMgr::construction(OptCtx & oc)
{
    reinit();
    m_rg->getPassMgr()->checkValidAndRecompute(&oc, PASS_DOM, PASS_UNDEF);

    //Extract dominate tree of CFG.
    START_TIMER(t, "PRSSA: Extract Dom Tree");
    DomTree domtree;
    m_cfg->genDomTree(domtree);
    END_TIMER(t, "PRSSA: Extract Dom Tree");

    if (!construction(domtree, oc)) {
        return;
    }
    set_valid(true);
    if (haveToMaintainClassicPRDU(oc)) {
        removeClassicPRDUForCallStmt(m_rg);
    }
    //The construction of PRSSA will destruct DUSet which built by DUMgr.
    oc.setInvalidPRDU();
}


//Note: Non-SSA DU Chains of read/write PR is unavaiable after SSA construction.
bool PRSSAMgr::construction(DomTree & domtree, OptCtx & oc)
{
    ASSERT0(m_rg);
    START_TIMER(t, "PRSSA: Build dominance frontier");
    DfMgr dfm;
    dfm.build((xcom::DGraph&)*m_cfg);
    END_TIMER(t, "PRSSA: Build dominance frontier");
    if (dfm.hasHighDFDensityVertex((xcom::DGraph&)*m_cfg)) {
        return false;
    }
    if (m_is_pruned) {
        m_livemgr = (LivenessMgr*)m_rg->getPassMgr()->
            registerPass(PASS_LIVENESS_MGR);
        m_livemgr->perform(oc);
        ASSERT0(m_livemgr->is_valid());
    } else {
        m_livemgr = nullptr;
    }
    DefMiscBitSetMgr sm;
    PRSet prset(sm.getSegMgr());
    BB2PRSet bb2definedprs(&sm);
    initMapInfo(sm, bb2definedprs, prset);
    placePhi(dfm, prset, *m_cfg->getBBList(), bb2definedprs);
    rename(prset, bb2definedprs, domtree);
    ASSERT0(verifyPhi(true, true) && verifyPrnoOfVPR());

    //Clean version stack after renaming.
    cleanPRNO2VPRStack();

    //Recompute the map if ssa needs reconstruct.
    cleanPRNO2Type();

    stripVersionForBBList(*m_cfg->getBBList());
    refinePhi(oc);
    if (m_livemgr != nullptr) {
        m_livemgr->clean();
    }
    if (g_dump_opt.isDumpAfterPass() && g_dump_opt.isDumpPRSSAMgr()) {
        START_TIMER(tdump, "PRSSA: Dump After Pass");
        dump();
        dfm.dump((xcom::DGraph&)*m_cfg, getRegion());
        END_TIMER(tdump, "PRSSA: Dump After Pass");
    }
    ASSERT0(verifyIRandBB(m_cfg->getBBList(), m_rg));
    ASSERT0(verifyPhi(false, false) && verifyVPR() && verifySSAInfo());
    set_valid(true);
    return true;
}
//END PRSSAMgr

} //namespace xoc
