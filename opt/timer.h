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
#ifndef _TIMER_H_
#define _TIMER_H_

namespace xoc {

//Specific Timer, show const string before timer start and end.
//User have to declare the 't' variable.
//e.g:
//    LONG t; //User declared
//    START_STIMER(t, "My Pass");
//    Run mypass();
//    END_STIMER(t, "My Pass");
#define START_STIMER(_timer_, s) \
    _timer_ = 0; \
    if (g_show_time) { \
        _timer_ = getclockstart(); \
        prt2C("\n==-- START %s", (s)); \
    }
#define END_STIMER(_timer_, s) \
    if (g_show_time) { \
        prt2C("\n==-- END %s", s); \
        prt2C(" Time:%fsec", getclockend(_timer_)); \
    }


//Timer, show const string before timer start and end.
//e.g:
//    START_TIMER(t, "My Pass");
//    Run mypass();
//    END_TIMER(t, "My Pass");
#define START_TIMER(_timer_, s) \
    LONG _timer_ = 0; \
    if (g_show_time) { \
        _timer_ = getclockstart(); \
        prt2C("\n==-- START %s", (s)); \
    }
#define END_TIMER(_timer_, s) \
    if (g_show_time) { \
        prt2C("\n==-- END %s", s); \
        prt2C(" Time:%fsec", getclockend(_timer_)); \
    }


//Timer, show format string before timer start and end.
//e.g:START_TIMER(t, ("My Pass Name:%s", getPassName()));
//    Run mypass();
//    END_TIMER(t, ("My Pass Name:%s", getPassName()));
#define START_TIMER_FMT(_timer_, s) \
    LONG _timer_ = 0; \
    if (g_show_time) { \
        _timer_ = getclockstart(); \
        prt2C("\n==-- START "); \
        prt2C s; \
    }
#define END_TIMER_FMT(_timer_, s) \
    if (g_show_time) { \
        prt2C("\n==-- END "); \
        prt2C s; \
        prt2C(" Time:%fsec", getclockend(_timer_)); \
    }

} //namespace xoc
#endif
