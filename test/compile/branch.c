void printf(char const*,...);
int br1(int *a, int *b, int *c, int n)
{
    int i = 0;
	while (i < n) {
		if (i > 4) {
			printf("\njump to L1");
			goto L1;
		}
L2:
		int j = i<0?0:i;
    	c[j] = a[j];
		i++;
		
	}
	while (i < n) {
		L1:
		if (i < 0) {
			printf("\njump to L2");
			goto L2;	
		}
		int j = i<0?0:i;
    	c[j] = a[j];
		i--;
	}
    return *a;
}
