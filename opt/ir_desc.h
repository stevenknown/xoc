/*@
Copyright (c) 2013-2021, Su Zhenyu steven.known@gmail.com

All rights reserved.

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

THIS SOFTWARE IS PROVIDED "AS IS" AND ANY
EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE CONTRIBUTORS BE LIABLE FOR ANY
DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
@*/
class IR;
class IRBB;

//Describe miscellaneous information for IR.
#define IRT_IS_STMT 0x1 //statement.
#define IRT_IS_BIN 0x2 //binary operation.
#define IRT_IS_UNA 0x4 //unary operation.

//Memory reference operation. Memory reference indicates all
//operations which write or load memory object.
#define IRT_IS_MEM_REF 0x8

//Memory operand indicates all operations which only load memory object.
#define IRT_IS_MEM_OPND 0x10

#define IRT_IS_ASSOCIATIVE 0x20
#define IRT_IS_COMMUTATIVE 0x40
#define IRT_IS_RELATION 0x80
#define IRT_IS_LOGICAL 0x100
#define IRT_IS_LEAF 0x200
#define IRT_HAS_RESULT 0x400
#define IRT_IS_STMT_IN_BB 0x800
#define IRT_IS_NON_PR_MEMREF 0x1000
#define IRT_HAS_DU 0x2000
#define IRT_WRITE_PR 0x4000
#define IRT_WRITE_WHOLE_PR 0x8000
#define IRT_HAS_OFFSET 0x10000
#define IRT_HAS_IDINFO 0x20000

#define IRDES_code(m) ((m).code)
#define IRDES_name(m) ((m).name)
#define IRDES_kid_map(m) ((m).kid_map)
#define IRDES_kid_num(m) ((m).kid_num)
#define IRDES_is_stmt(m) (HAVE_FLAG(((m).attr), IRT_IS_STMT))
#define IRDES_is_bin(m) (HAVE_FLAG(((m).attr), IRT_IS_BIN))
#define IRDES_is_una(m) (HAVE_FLAG(((m).attr), IRT_IS_UNA))
#define IRDES_is_mem_ref(m) (HAVE_FLAG(((m).attr), IRT_IS_MEM_REF))
#define IRDES_is_mem_opnd(m) (HAVE_FLAG(((m).attr), IRT_IS_MEM_OPND))
#define IRDES_is_associative(m) (HAVE_FLAG(((m).attr), IRT_IS_ASSOCIATIVE))
#define IRDES_is_commutative(m) (HAVE_FLAG(((m).attr), IRT_IS_COMMUTATIVE))
#define IRDES_is_relation(m) (HAVE_FLAG(((m).attr), IRT_IS_RELATION))
#define IRDES_is_logical(m) (HAVE_FLAG(((m).attr), IRT_IS_LOGICAL))
#define IRDES_is_leaf(m) (HAVE_FLAG(((m).attr), IRT_IS_LEAF))
#define IRDES_is_stmt_in_bb(m) (HAVE_FLAG(((m).attr), IRT_IS_STMT_IN_BB))
#define IRDES_is_non_pr_memref(m) (HAVE_FLAG(((m).attr), IRT_IS_NON_PR_MEMREF))
#define IRDES_has_result(m) (HAVE_FLAG(((m).attr), IRT_HAS_RESULT))
#define IRDES_has_offset(m) (HAVE_FLAG(((m).attr), IRT_HAS_OFFSET))
#define IRDES_has_idinfo(m) (HAVE_FLAG(((m).attr), IRT_HAS_IDINFO))
#define IRDES_has_du(m) (HAVE_FLAG(((m).attr), IRT_HAS_DU))
#define IRDES_is_write_pr(m) (HAVE_FLAG(((m).attr), IRT_WRITE_PR))
#define IRDES_is_write_whole_pr(m) (HAVE_FLAG(((m).attr), IRT_WRITE_WHOLE_PR))
#define IRDES_size(m) ((m).size)
class IRDesc {
public:
    //NOTE: Do NOT change the layout of class members because they are
    //corresponding to the special initializing value.
    IR_TYPE code;
    CHAR const* name;
    BYTE kid_map;
    BYTE kid_num;
    BYTE size;
    UINT attr;
public:
    static bool mustExist(IR_TYPE irtype, UINT kididx);
};


//Defined rounding type that CVT operation used.
typedef enum _ROUND_TYPE {
    ROUND_UNDEF = 0,

    //Rounding down (or take the floor, or round towards minus infinity)
    ROUND_DOWN,

    //Rounding up (or take the ceiling, or round towards plus infinity)
    ROUND_UP,

    //Rounding towards zero (or truncate, or round away from infinity)
    ROUND_TOWARDS_ZERO,

    //Rounding away from zero (or round towards infinity)
    ROUND_AWAY_FROM_ZERO,

    //Rounding to the nearest integer
    ROUND_TO_NEAREST_INTEGER,

    //Rounding half up
    ROUND_HALF_UP,

    //Rounding half down
    ROUND_HALF_DOWN,

    //Rounding half towards zero
    ROUND_HALF_TOWARDS_ZERO,

    //Rounding half away from zero
    ROUND_HALF_AWAY_FROM_ZERO,

    //Rounding half to even
    ROUND_HALF_TO_EVEN,

    //Rounding half to odd
    ROUND_HALF_TO_ODD,
    ROUND_TYPE_NUM,
} ROUND_TYPE;

#define ROUND_NAME(r) (ROUNDDESC_name(g_round_desc[(r)]))
#define ROUNDDESC_type(r) ((r).type)
#define ROUNDDESC_name(r) ((r).name)
class RoundDesc {
public:
    //Note: do not change the layout of members because they are
    //corresponding to the special initializing value.
    ROUND_TYPE type;
    CHAR const* name;
};

//Exported Variables.
extern IRDesc const g_ir_desc[];
extern RoundDesc const g_round_desc[];
