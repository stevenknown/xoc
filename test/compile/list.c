struct node {
    struct node * np;
    int min;
    int max;
};
void walk(struct node * p, int m)
{
    struct node * q;
    for (q = p; !q; q = q->np) {
        if (q->max >= m) {
            break;
        }
    }
}
