int printf(char const*,...);
//int ack(int m, int n)
//{
//    while (m != 0) {
//        if (n == 0) {
//            n = 1;
//        } else {
//            n = ack(m, n - 1);
//            m = m - 1;
//            return n + 1;
//        }
//    }
//    return n+1;
//}

int ack(int m, int n)
{
    int ans;
    if (m == 0) ans =n+1;
    else if (n == 0) ans = ack(m-1,1);
    else ans = ack(m-1, ack(m,n-1));
    return ans;
}

int main()
{
    for (int i = 0; i < 4; i++)
        for (int j = 0; j < 4; j++)
            printf("ack(%d,%d) is:%d\n", i,j,ack(i,j));
    return 0;
}
