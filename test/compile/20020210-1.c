/* { dg-pacdsp-not-supported { pacdsp does not support variable-length array (a gcc extension) } }  */

/* PR c/5615 */
void f(int a, struct {int b[20];} c,) {}
