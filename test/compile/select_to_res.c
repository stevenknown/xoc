int dp[4 +1][8 +1];
int e;
int foo()
{
    int weight[]={0,3,4,5,2};
    int val[]= {0,5,6,7,4};
    for (int curtotalw = 1; curtotalw <= 8; curtotalw++) {
        int val_notake = dp[e-1][curtotalw];
        int val_take = dp[e-1][curtotalw - weight[e]] + val[e];
        int val_max = ((val_take)>(val_notake)?(val_take):(val_notake));
        dp[e][curtotalw] = val_max;
    }
    return 0;
}
