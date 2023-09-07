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
#include "../com/xcominc.h"
#include "../opt/cominc.h"
#include "ir_lex.h"
#include "xcode.h"
#include "ir_parser.h"
#include "grreader.h"

namespace xoc {

GRReader::GRReader(RegionMgr * rumgr)
{
    m_rumgr = rumgr;
    m_lexer = new Lexer();
    m_parser = new IRParser(rumgr);
    m_parser->setLexer(m_lexer);
}


GRReader::~GRReader()
{
    delete m_lexer;
    delete m_parser;
}


void GRReader::setSrcFile(FILE * h)
{
    getLexer()->setSrcFile(h);
}


bool GRReader::parse()
{
    getLexer()->clean();
    return getParser()->parse();
}


//Read IR from gr file.
//Return true if no error find.
bool readGRAndConstructRegion(RegionMgr * rumgr, CHAR const* grfile)
{
    START_TIMER(t, "readGRAndConstructRegion");
    GRReader reader(rumgr);

    //START_TIMER(t, "lexer dump");
    //reader.getLexer()->dump(grfile, nullptr);
    //END_TIMER(t, "lexer dump");
    FO_STATUS st;
    xcom::FileObj fo(grfile, false, true, &st);
    if (st != FO_SUCC) { return false; }
    ASSERT0(fo.getFileHandler());
    reader.setSrcFile(fo.getFileHandler());
    bool succ = reader.parse();
    END_TIMER(t, "readGRAndConstructRegion");
    return succ;
}

} //namespace xoc
