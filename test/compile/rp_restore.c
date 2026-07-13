double x[10];
void load_elim(int i)
{
    for (int d = 1; d <= i; d++) {
        x[0] = x[0];
        if (d > 100) {
            break;
        }
        x[4] = d;
    }
}
