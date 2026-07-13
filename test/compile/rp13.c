void foo()
{
    int i;
    char C[2];
    char A[10];

    //only i can be promoted.
    for (i = 0; i != 10; ++i) {
    	  ((short*)C)[0] = A[i]; //Two byte store!, so it is alias with C[1].
    	  C[1] = A[9-i];         //One byte store
    }
}
