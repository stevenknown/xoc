enum
{
  TIMED_NP,
  RECURSIVE_NP,
  ERRORCHECK_NP,
  ADAPTIVE_NP
  ,
  NORMAL = TIMED_NP,
  RECURSIVE = RECURSIVE_NP,
  ERRORCHECK = ERRORCHECK_NP,
  DEFAULT = NORMAL
  /* For compatibility.  */
  , FAST_NP = TIMED_NP,
  PICK = RECURSIVE
};

enum
{
  E1 = 4,
  E2, 
  E3 = E1,
};

int main()
{
    if (E1 != 4) { return 1; }
    if (E2 != 5) { return 3; }
    if (E3 != E1) { return 2; }

    if (0 != TIMED_NP) { return 6; }
    if (NORMAL != TIMED_NP) { return 4; }
    if (DEFAULT != NORMAL) { return 5; }
    if (PICK != RECURSIVE) { return 7; }
    if (PICK != 1) { return 8; }
    if (FAST_NP != 0) { return 9; }

    return 0;
}
