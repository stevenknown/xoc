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
#include "ltype.h"
#include "comf.h"
#include "smempool.h"
#include "sstl.h"
#include "matt.h"
#include "bs.h"
#include "sbs.h"
#include "sgraph.h"
#include "agraph.h"

namespace xcom {

//Build matrix to describe edge-weight.
void AGraph::buildAdjacentMatrix(Matrix<UINT> & adj_mat)
{
    DUMMYUSE(adj_mat);
    ASSERT(0, ("Target Dependent Code"));
    //Like: adj_mat.set(i, j, Edge-WEIGHT);
}


/* Nonrecursive algo to compute the shorest-path.
See <Shortest_Path-Dijkstra.txt>
'infinite': the value indiates infinity. */
void AGraph::ShortestPath(UINT infinite)
{
    ASSERT(m_ec_pool != NULL, ("not yet initialized."));
    Matrix<UINT> adj_mat;
    buildAdjacentMatrix(adj_mat);
    UINT row = adj_mat.get_row_size();
    UINT col = adj_mat.get_col_size();
    UINT i,j,k,min,v1,v2;
    if (m_spath_mat != NULL) { delete m_spath_mat; }
    m_spath_mat = new Matrix<UINT>(row, col);
    m_spath_mat->set_all(infinite);

    //Init path matrix
    for (i = 0; i < row; i++) {
        for (j = 0; j < col; j++) {
            v1 = adj_mat.get(i,j);
            if (v1 != infinite) {
                m_spath_mat->set(i, j, i);
            }
        }
    }
    for (i = 0; i < row; i++) {
        min = infinite;
        for (j = 0; j < col; j++) {
            for (k = 0; k < row; k++) {
                v1 = adj_mat.get(k, j);
                v2 = adj_mat.get(i, k);
                if ((v1 != infinite) && (v2 != infinite)) {
                    if (min > (v1 + v2)) {
                        min = v1 + v2;
                        m_spath_mat->set(i, j, k);
                    }
                }
            }
            ASSERT(min <= infinite, ("exception occur in ShortestPath"));
            if (min != infinite) {
                adj_mat.set(i, j, min);
            }
            min = infinite;
        }
    }
}

} //namespace xcom
