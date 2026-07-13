struct node {
    struct node * np;
    int max;
};
//#pragma xxxx, yyyy
void inbounds()
{
#pragma hmpp <grp_label> [codelet_label]? directive_type [,directive_parameters]* [&]
    struct node * q;
    int i;
    q = q->np;
} 
