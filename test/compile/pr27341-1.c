extern double R_NaReal;
void z_atan2 (double * r, double * ccs)
{
    if (*ccs == 0)
        *r = R_NaReal;
}

