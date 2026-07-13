
/*
 *This trivial function returns always 1.
 Compiling with clang with -O2 option and looking at the
 disassembled code LLVM correctly compiles this function
 as return 1;.
 Get a dump
 clang -c -mllvm -print-after-all -O2 foo.c

If you convert it to SSA form, it looks something like this:
  // block 1
  if (c == 0) goto L1;
    // block 2
    a_0 = 1;
    b_0 = 1;
    d_0 = a_0;
    goto L2;
L1: //else
    // block 3
    a_1 = 2;
    b_1 = 2;
    d_1 = 1;
    goto L2;

L2:
  // block 4 (has two possible predecessors: block 2 or block 3)
  a_2 = PHI(a_0, a_1); // i.e. a_0 if we came from block 2, a_1 if we came from block 3
  b_2 = PHI(b_0, b_1); // i.e. b_0 if we came from block 2, b_1 if we came from block 3
  d_2 = PHI(d_0, d_1); // i.e. d_0 if we came from block 2, d_1 if we came from block 3
  if (a_2 != b_2) goto L3;
  // block 5
  x_0 = d_2;
  goto L4:
L3:
  // block 6
  x_1 = 0;
  goto L4;

L4:
  // block 7 (has two possible predecessors: block 5 or block 6)
  return PHI(x_0, x_1); // i.e. x_0 if we came from block 5, x_1 if we came from block 6


Simply propagation of constant values results in this:
  // block 1
  if (c == 0) goto L1;
  // block 2
  a_0 = 1;
  b_0 = 1;
  d_0 = 1;
  goto L2;
L1:
  // block 3
  a_1 = 2;
  b_1 = 2;
  d_1 = 1;
  goto L2;

L2:
  // block 4
  a_2 = PHI(1, 2); // i.e. 1 if we came from block 2, 2 if we came from block 3
  b_2 = PHI(1, 2); // i.e. 1 if we came from block 2, 2 if we came from block 3
  d_2 = 1;         // PHI(d_0, d_1) == PHI(1, 1) i.e. 1 regardless of where we came from
                   // d_2 is defined but unused.
  if (a_2 != b_2) goto L3;
  // block 5
  x_0 = 1;  // (we've deduced that d_2 == 1 regardless of control flow)
  goto L4:
L3:
  // block 6
  x_1 = 0;
  goto L4;

L4:
  // block 7
  return PHI(1, 0); // i.e. 1 if we came from block 5, 0 if we came from block 6


Simplifying to remove the assignments which are no longer used for anything else gives this:

  // block 1
  if (c == 0) goto L1;
  // block 2
  goto L2;
L1:
  // block 3
  goto L2;

L2:
  // block 4
  a_2 = PHI(1, 2); // i.e. 1 if we came from block 2, 2 if we came from block 3
  b_2 = PHI(1, 2); // i.e. 1 if we came from block 2, 2 if we came from block 3
  if (a_2 != b_2) goto L3;
  // block 5
  goto L4:
L3:
  // block 6
  goto L4;

L4:
  // block 7
  return PHI(1, 0); // i.e. 1 if we came from block 5, 0 if we came from block 6

Because a_2 is definitly equal to b_2, so we came from block 5 always. So return 1.
*/
int c;

//Version 1 : all variable are global variable. in this case
//All a,b,d,x need to restore to memory.
int a, b, d, x, e;
int just_return_1()

//Version 2 : all variable are parameters. in this case
//Only x needs to restore to memory.
//int just_return_1(int a, int b, int d, int x)
{
  while (e) {
      if (c) {
        a = 1;
        b = 1;
        d = a;
      } else {
        a = 2;
        b = 2;
        d = 1;
      }

      if (a == b) {
        x = d;
      } else {
        x = 0;
      }
  }
  return x;
}
