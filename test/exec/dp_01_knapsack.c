#include "stdio.h"

#define maxw 8
#define itemnum 4
#define MAX(a,b) ((a)>(b)?(a):(b))

int dp[itemnum+1][maxw+1];

int main()
{
    int weight[]={0,3,4,5,2};
    int val[]=   {0,5,6,7,4};

    for (int w = 0; w <= maxw; w++) {
        dp[0][w] = 0;
    }

    for (int e = 1; e <= itemnum; e++) {
        for (int curtotalw = 1; curtotalw <= maxw; curtotalw++) {
            if (weight[e] > curtotalw) {
                //Backpack can't hold weight[e].
                dp[e][curtotalw] = dp[e-1][curtotalw]; //notake elem e.
                continue;
            }
            int val_notake = dp[e-1][curtotalw];
            int val_take   = dp[e-1][curtotalw - weight[e]] + val[e];
            int val_max = MAX(val_take, val_notake);
            dp[e][curtotalw] = val_max;
        }
    }
    char const* f1 = "%-6s ";
    char const* f2 = "%-4s ";
    char const* f3 = "%04d ";
    printf("\n");
    printf(f1, " ");
    char buf[128];
    for (int j = 0; j <= maxw; j++) {
        sprintf(buf, "w=%d", j);
        printf(f2, buf);
    }
    for (int i = 0; i <= itemnum; i++) {
        printf("\n");
        if (i > 0) {
            sprintf(buf, "item%d", i);
            printf(f1, buf);
        } else {
            printf(f1, " ");
        }
        for (int j = 0; j <= maxw; j++) {
            printf(f3, dp[i][j]);
        } 
    }
    printf("\n");
    return 0;
}
