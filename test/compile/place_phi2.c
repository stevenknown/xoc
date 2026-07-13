int g;
int w;
void compute_dominance_frontier(int x, int y)
{
    //This is a classic test case for DF verification, see figure at
    //P26 in new_new/ssa/Static Single Assignment Form.pdf

    //Ths Dominace Frontier of BB22 should be {BB22, BB42, BB45, BB47}
    char z;
    switch (x) {
    case 0:
        z = 100;
        do {
            z--;
        } while (z > 0);
        goto join_point_4;

    case 1:
        do {
            z = 200;
            if (y > z) {
                y = 199;
                if (g > y) {
                    goto join_point_4;
                }
            } else {
                y = 201;
                if (g < y) {
                    goto join_point_12;
                }
            }
            z--;
        } while (z > 0);
        goto join_point_13;

    case 2:
        if (y == 19) {
            z = 119;
        } else {
            z = 911;
        }
        join_point_12:
        w = z;
        goto join_point_13;
    }

join_point_4:
    w = 33;
    goto join_point_13;

join_point_13:
    return w;
}
