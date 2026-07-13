#define _INC_SETJMP

// Definitions specific to particular setjmp implementations.
#if defined _M_IX86

    #define _JBLEN  16
    #define _JBTYPE int

    typedef struct __JUMP_BUFFER
    {
        unsigned long Ebp;
        unsigned long Ebx;
        unsigned long Edi;
        unsigned long Esi;
        unsigned long Esp;
        unsigned long Eip;
        unsigned long Registration;
        unsigned long TryLevel;
        unsigned long Cookie;
        unsigned long UnwindFunc;
        unsigned long UnwindData[6];
    } _JUMP_BUFFER;

#elif defined _M_X64

    typedef struct _SETJMP_FLOAT128
    {
        unsigned long long Part[2];
    } SETJMP_FLOAT128;

    #define _JBLEN  16
    typedef SETJMP_FLOAT128 _JBTYPE;

    typedef struct _JUMP_BUFFER
    {
        unsigned long long Frame;
        unsigned long long Rbx;
        unsigned long long Rsp;
        unsigned long long Rbp;
        unsigned long long Rsi;
        unsigned long long Rdi;
        unsigned long long R12;
        unsigned long long R13;
        unsigned long long R14;
        unsigned long long R15;
        unsigned long long Rip;
        unsigned long MxCsr;
        unsigned short FpCsr;
        unsigned short Spare;

        SETJMP_FLOAT128 Xmm6;
        SETJMP_FLOAT128 Xmm7;
        SETJMP_FLOAT128 Xmm8;
        SETJMP_FLOAT128 Xmm9;
        SETJMP_FLOAT128 Xmm10;
        SETJMP_FLOAT128 Xmm11;
        SETJMP_FLOAT128 Xmm12;
        SETJMP_FLOAT128 Xmm13;
        SETJMP_FLOAT128 Xmm14;
        SETJMP_FLOAT128 Xmm15;
    } _JUMP_BUFFER;

#elif defined _M_ARM

    #define _JBLEN  28
    #define _JBTYPE int

    typedef struct _JUMP_BUFFER
    {
        unsigned long Frame;

        unsigned long R4;
        unsigned long R5;
        unsigned long R6;
        unsigned long R7;
        unsigned long R8;
        unsigned long R9;
        unsigned long R10;
        unsigned long R11;

        unsigned long Sp;
        unsigned long Pc;
        unsigned long Fpscr;
        unsigned long long D[8]; // D8-D15 VFP/NEON regs
    } _JUMP_BUFFER;

#elif defined _M_ARM64

    #define _JBLEN  24
    #define _JBTYPE unsigned long long

    typedef struct _JUMP_BUFFER {
        unsigned long long Frame;
        unsigned long long Reserved;
        unsigned long long X19;   // x19 -- x28: callee saved registers
        unsigned long long X20;
        unsigned long long X21;
        unsigned long long X22;
        unsigned long long X23;
        unsigned long long X24;
        unsigned long long X25;
        unsigned long long X26;
        unsigned long long X27;
        unsigned long long X28;
        unsigned long long Fp;    // x29 frame pointer
        unsigned long long Lr;    // x30 link register
        unsigned long long Sp;    // x31 stack pointer
        unsigned int Fpcr;  // fp control register
        unsigned int Fpsr;  // fp status register

        double D[8]; // D8-D15 FP regs
    } _JUMP_BUFFER;

#else
    //Default target is ARM.
    #define _JBLEN  28
    #define _JBTYPE int

    typedef struct _JUMP_BUFFER
    {
        unsigned long Frame;

        unsigned long R4;
        unsigned long R5;
        unsigned long R6;
        unsigned long R7;
        unsigned long R8;
        unsigned long R9;
        unsigned long R10;
        unsigned long R11;

        unsigned long Sp;
        unsigned long Pc;
        unsigned long Fpscr;
        unsigned long long D[8]; // D8-D15 VFP/NEON regs
    } _JUMP_BUFFER;

#endif

// Define the buffer type for holding the state information
#ifndef _JMP_BUF_DEFINED
    #define _JMP_BUF_DEFINED
    typedef _JBTYPE jmp_buf[_JBLEN];
#endif

typedef jmp_buf sigjmp_buf;

// Function prototypes
int setjmp(jmp_buf _Buf);
void longjmp(jmp_buf _Buf, int _Value);
int sigsetjmp(sigjmp_buf env, int savesigs);
void siglongjmp(sigjmp_buf env, int val);
