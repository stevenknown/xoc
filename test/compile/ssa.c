void out_of_ssa()
{
    int x;
    x = 1; //x1
    do {
        //x2 = phi(x1, x3)
        x = x+1; //x3 = x2 + 1
    } while (x < 100); //x3
    return x; //x3? x2?
}
