typedef struct _atype
{
    float bg[1], cg[1];
    bool ant;
}atype;


void cp_assert(bool*, float*, int*, bool*);

void f(atype **rng_stream, int *error, float u)
{
    bool t = *rng_stream != 0;
    float routinep;
    bool failure;
    cp_assert ( &t, &routinep, error, &failure);
    if (failure == 0)
    {
        typedef float ty[1];
        ty *tt = &((*rng_stream)->bg);
        int i = 1;

        do
        {
            (*tt)[i - 1] = u;
            i ++;
        }while (i > 1);
        {
            ty *tt = &(*rng_stream)->cg;
            int i = 1;

            do
            {
                (*tt)[i - 1] = u;
                i ++;
            }while (i > 1);
        }
    }
}


