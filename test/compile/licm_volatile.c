//The case is used to show how DCE optimize the Dead Control Flow Structure.
typedef struct _tag { int a; } pthread_t;
void sleep(int);
void printf(char const* fmt, ...);
void perror(char const* fmt);
int pthread_create(pthread_t*, char*,  void (*thread1)(void *), char*);
static int tcnt = 1;
static volatile int tcnt2 = 1;
void * thread1(void *){
    sleep(2);
    printf("enter\n");
    tcnt = tcnt == -1 ? 0 : -1;
    tcnt2 = tcnt2 == -1 ? 0 : -1;
    return 0;
}
int main() {
    pthread_t t;
    int re = pthread_create(&t, 0, &thread1, 0);
    if(re < 0){
        perror("thread");
    }
	//Can be LICM.
    while(tcnt > 0){
    }
	//Can not be LICM, but can be DCE.
    while(tcnt2 > 0){
    }
    return 0;
}
