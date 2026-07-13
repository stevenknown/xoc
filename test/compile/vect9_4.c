float x;
void foo(float * restrict a, float * restrict b, int start, int end) {
  //End bound is variable.
  for (int i = 0; i < end; i++) {
      //The loop can be vectorized.
      //ld a, ld b have different livein VN.
      a[i] *= b[i] + x;
  }
}
