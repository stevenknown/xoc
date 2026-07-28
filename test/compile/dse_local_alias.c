int *p;
int t2;
void dse(int g, int n)
{
    g = 0; //it can be DSEed even if there is an alias-store.
           //The alias-store has no impact on the DSE.
    int i;
    if (n > 0) { p = &g; }
    else {
        ; //p may point to global-var.
    }
    i = 0;
    while (i < t2) {
        g = 3; //cannot be DSE. next-def is aliased may-overlap-def.
        i++;
        *p=6;
        g = 2;
    }
    g = 4;
}
 
