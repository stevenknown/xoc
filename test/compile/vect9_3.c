void foo(float * restrict a, float * restrict b, int num) {
  float x;
  //End bound is variable.
  for (int i = 0; i < 100; i++) {
      //The loop can be vectorized.
      //ld a, ld b have different livein VN.
      a[i] *= b[i] + x;
  }
}
