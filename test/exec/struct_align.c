typedef struct _taga
{
   char        c;
   short int   s;
} A;

typedef struct _tagb
{
   short int   s;
   char        c;
   int         i;
} B;

typedef struct _tagc
{
   char        c;
   double      d;
   int         s;
} C;

typedef struct _tagd
{
   double      d;
   int         s;
   char        c;
} D;

typedef struct _tage
{
   double      d;
   int         s;
   char        c[37];
} E;

typedef struct _tagf
{
   char        c;
   char        d[7];
   int         s;
} F;

typedef struct _tagg
{
   double      c;
   char        d[7];
   int         s;
} G;

int main()
{
   if (sizeof(A) != 4) { return 1; }
   if (sizeof(B) != 8) { return 2; }
   if (sizeof(C) != 24) { return 3; }
   if (sizeof(D) != 16) { return 4; }
   if (sizeof(E) != 56) { return 5; }
   if (sizeof(F) != 12) { return 6; }
   if (sizeof(G) != 24) { return 7; }
   return 0;
}

