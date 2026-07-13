int compute_dominance_frontier(int w)
{
    char z = 200;
    do {
        if (w > 0) {
            z--; //z is not IV.
        }
    } while (z > 0);
    return z;
}
