void printf(char const*,...);
struct defs
{
  int cbits;			/* No. of bits per char           */
  int ibits;			/*                 int            */
  int sbits;			/*                 short          */
  int lbits;			/*                 long           */
  int ubits;			/*                 unsigned       */
  int fbits;			/*                 float          */
  int dbits;			/*                 double         */
  int fprec;			/* Smallest number that can be    */
  int dprec;			/* significantly added to 1.      */
  int flgs;			/* Print return codes, by section */
  int flgm;			/* Announce machine dependencies  */
  int flgd;			/* give explicit diagnostics      */
  int flgl;			/* Report local return codes.     */
  int rrc;			/* recent return code             */
  int crc;			/* Cumulative return code         */
  char rfs[8];			/* Return from section            */
};

int s61 (struct defs * pd0)		/* Characters and integers */
{
  static char s61er[] = "s61,er%d\n";
  static char qs61[8] = "s61    ";
  short from, shortint;
  long int to, longint;
  int rc, lrc;
  int j;
  char fromc, charint;
  char *wd, *pc[6];


  char *ps, *pt;

  lrc = 0;
    for (j = 0; j < 6; j++)
      while (*pc[j])
  //    if (*pc[j]++ < 0)
          lrc = 1;

  if (lrc != 0)
    {
      rc = rc + 2;
      if (pd0->flgd != 0)
        printf (s61er, 2);
    }

  return rc;
}
