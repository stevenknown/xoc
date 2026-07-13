//////////////////////
//This case is to show how dce optimize the Dead Control Flow Structure.
//Such as the loop of main(), if vvv is not volatile, it will be removed.
typedef struct _tag { int a; } pthread_t;
void sleep(int);
void printf(char const* fmt, ...);
void perror(char const* fmt);
int pthread_create(pthread_t*, char*,  void (*thread1)(void *), char*);

static int vvv = 1; //Wrong! Dead loop removed.
//static volatile int vvv = 1; //It is correctly! Dead loop will not removed.
void * thread1(void *){
    sleep(2);
    printf("enter\n");
    vvv = -1;
    return 0;
}
int main() {
    pthread_t t;
    int re = pthread_create(&t, 0, &thread1, 0);
    if(re < 0){
        perror("thread");
    }

	//Here is the Dead Loop.
    while(vvv > 0){
    }
	//
    return 0;
}
//////////////////////
