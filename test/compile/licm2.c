typedef struct {
  unsigned num_words;
  unsigned word_ofs;
  const unsigned *data;
} section_t;

void test(section_t* restrict section, unsigned* restrict dst)
{
  while (section->data != 0) {
    const unsigned * restrict src = section->data;
    for (unsigned i=0; i < section->num_words; ++i) {
      /*
      There is no need to reload section->word_ofs on every
      iteration of the for loop, because src, dst, section have marked restrict.
      */
      dst[section->word_ofs + i] = src[i];
    }
    ++section;
  }
}

