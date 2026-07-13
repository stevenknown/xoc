typedef int sig_atomic_t;
typedef void (*_crt_signal_t)(int);

#define NSIG            23  // maximum signal number + 1

// Signal types
#define SIGINT          2   // interrupt
#define SIGILL          4   // illegal instruction - invalid function image
#define SIGFPE          8   // floating point exception
#define SIGSEGV         11  // segment violation
#define SIGTERM         15  // Software termination signal from kill
#define SIGBREAK        21  // Ctrl-Break sequence
#define SIGABRT         22  // abnormal termination triggered by abort call

#define SIGABRT_COMPAT  6   // SIGABRT compatible with other platforms, same as SIGABRT

// Signal action codes
#define SIG_DFL ((_crt_signal_t)0)     // default signal action
#define SIG_IGN ((_crt_signal_t)1)     // ignore signal
#define SIG_GET ((_crt_signal_t)2)     // return current value
#define SIG_SGE ((_crt_signal_t)3)     // signal gets error
#define SIG_ACK ((_crt_signal_t)4)     // acknowledge

void signal(int, void(*f)(int));
