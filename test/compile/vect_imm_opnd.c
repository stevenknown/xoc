extern double G1[100];
extern double G2[100];
void swap() {
  for (int j = 0; j < 147; j++) {
    G1[j] = G2[j] + 10;
  }
}

