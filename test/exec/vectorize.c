typedef unsigned short uint16_t;
typedef unsigned int uint32_t;
typedef unsigned long long uint64_t;
double time;
uint64_t time_us;
struct timeval {int tv_sec; int tv_usec;} time_start,time_end;
int gettimeofday(struct timeval * tp, struct timezone * tzp);
void * malloc(int size);
void free(void*);
int printf(char const*,...);
int main()
{
	const uint16_t *buffer;
	buffer = (uint16_t*)malloc(sizeof(uint16_t)*(1<<8));
	uint32_t* p = (uint32_t*)buffer;
	p = (uint32_t*)buffer;
	int m = 0;
	m = 0;
    uint16_t x1,x2;
	gettimeofday(&time_start,0);
    int j;
	for(j = 0;j< (1<<23);j++)
	{
		p = (uint32_t*)buffer;
        int i;
		for(i = 0;i< (1<<8);i = i + 2)
		{
			x1 = *(uint16_t*)p;
			x2 = *((uint16_t*)p + 1);
			m = m + x1+x2;
			p++;
		}
	}
	gettimeofday(&time_end,0);
	time_us = 1000000*(time_end.tv_sec-time_start.tv_sec) + time_end.tv_usec - time_start.tv_usec;
	printf("%ldus\n",time_us);
	free((void*)buffer);
	printf("m = %d \n",m);
	return 0;
}
