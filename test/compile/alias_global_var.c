typedef unsigned int size_t;
typedef int wchar_t;
typedef int _LOCK_T;
typedef int _LOCK_RECURSIVE_T;
typedef long _off_t;
typedef long long _off64_t;
typedef int _ssize_t;
typedef unsigned int wint_t;
typedef struct
{
  int __count;
  union
  {
    wint_t __wch;
    unsigned char __wchb[4];
  } __value; /* Value so far.  */
} _mbstate_t;
typedef _LOCK_RECURSIVE_T _flock_t;
typedef void *_iconv_t;
typedef unsigned long __ULong;
struct _Bigint
{
  struct _Bigint *_next;
  int _k, _maxwds, _sign, _wds;
  __ULong _x[1];
};
struct __tm
{
  int __tm_sec;
  int __tm_min;
  int __tm_hour;
  int __tm_mday;
  int __tm_mon;
  int __tm_year;
  int __tm_wday;
  int __tm_yday;
  int __tm_isdst;
};
struct _on_exit_args {
 void * _fnargs[32];
 void * _dso_handle[32];
 __ULong _fntypes;
 __ULong _is_cxa;
};
struct _atexit {
 struct _atexit *_next;
 int _ind;
 void (*_fns[32])(void);
 struct _on_exit_args * _on_exit_args_ptr;
};
struct __sbuf {
 unsigned char *_base;
 int _size;
};
typedef long _fpos_t;
struct __sFILE_fake {
  unsigned char *_p;
  int _r;
  int _w;
  short _flags;
  short _file;
  struct __sbuf _bf;
  int _lbfsize;
  struct _reent *_data;
};
struct __sFILE {
  unsigned char *_p;
  int _r;
  int _w;
  short _flags;
  short _file;
  struct __sbuf _bf;
  int _lbfsize;
  struct _reent *_data;
  void * _cookie;
  int (*_read) (void * _cookie, char *_buf, int _n);
  int (*_write) (void * _cookie, const char *_buf, int _n);
  _fpos_t (*_seek) (void * _cookie, _fpos_t _offset, int _whence);
  int (*_close) (void * _cookie);
  struct __sbuf _ub;
  unsigned char *_up;
  int _ur;
  unsigned char _ubuf[3]; /* guarantee an ungetc() buffer */
  unsigned char _nbuf[1]; /* guarantee a getc() buffer */
  /* separate buffer for fgetline() when line crosses buffer boundary */
  struct __sbuf _lb; /* buffer for fgetline() */
  /* Unix stdio files get aligned to block boundaries on fseek() */
  int _blksize; /* stat.st_blksize (may be != _bf._size) */
  int _offset; /* current lseek offset */
};
typedef struct __sFILE __FILE;
struct _glue
{
  struct _glue *_next;
  int _niobs;
  __FILE *_iobs;
};
struct _rand48 {
  unsigned short _seed[3];
  unsigned short _mult[3];
  unsigned short _add;
  unsigned long long _rand_next;
};
struct _mprec
{
  struct _Bigint *_result;
  int _result_k;
  struct _Bigint *_p5s;
  struct _Bigint **_freelist;
};
struct _misc_reent
{
  char *_strtok_last;
  _mbstate_t _mblen_state;
  _mbstate_t _wctomb_state;
  _mbstate_t _mbtowc_state;
  char _l64a_buf[8];
  int _getdate_err;
  _mbstate_t _mbrlen_state;
  _mbstate_t _mbrtowc_state;
  _mbstate_t _mbsrtowcs_state;
  _mbstate_t _wcrtomb_state;
  _mbstate_t _wcsrtombs_state;
};
struct _reent
{
  __FILE *_stdin, *_stdout, *_stderr; /* XXX */
  int _errno; /* local copy of errno */
  int _inc; /* used by tmpnam */
  char *_emergency;
  int __sdidinit; /* 1 means stdio has been init'd */
  int _current_category; /* used by setlocale */
  const char *_current_locale;
  struct _mprec *_mp;
  void (*__cleanup) (struct _reent *);
  int _gamma_signgam;
  int _cvtlen;
  char *_cvtbuf;
  struct _rand48 *_r48;
  struct __tm *_localtime_buf;
  char *_asctime_buf;
  void (**(_sig_func))(int);
  struct _atexit *_atexit;
  struct _atexit _atexit0;
  struct _glue __sglue; /* root of glue chain */
  __FILE *__sf; /* file descriptors */
  struct _misc_reent *_misc; /* strtok, multibyte states */
  char *_signal_buf; /* strsignal */
};
extern const struct __sFILE_fake __sf_fake_stdin;
extern const struct __sFILE_fake __sf_fake_stdout;
extern const struct __sFILE_fake __sf_fake_stderr;
extern struct _reent *_impure_ptr ;
extern struct _reent *const _global_impure_ptr ;
void _reclaim_reent (struct _reent *);
typedef struct
{
  int quot; /* quotient */
  int rem; /* remainder */
} div_t;
typedef struct
{
  long quot; /* quotient */
  long rem; /* remainder */
} ldiv_t;
typedef struct
{
  long long int quot; /* quotient */
  long long int rem; /* remainder */
} lldiv_t;
extern int __mb_cur_max;
void abort(void);
int abs(int);
int atexit (void (*__func)(void));
double atof (const char *__nptr);
float atoff (const char *__nptr);
int atoi (const char *__nptr);
int _atoi_r (struct _reent *, const char *__nptr);
long atol (const char *__nptr);
long _atol_r (struct _reent *, const char *__nptr);
void * bsearch (const void * __key, const void * __base, size_t __nmemb, size_t __size, int (* _compar) (const void *, const void *));
void * calloc (size_t __nmemb, size_t __size);
div_t div (int __numer, int __denom);
void exit (int __status);
void free (void *);
char * getenv (const char *__string);
char * _getenv_r (struct _reent *, const char *__string);
char * _findenv (const char *, int *);
char * _findenv_r (struct _reent *, const char *, int *);
long labs (long);
ldiv_t ldiv (long __numer, long __denom);
void * malloc (size_t __size);
int mblen (const char *, size_t);
int _mblen_r (struct _reent * a, const char * b, size_t c, _mbstate_t * d);
int mbtowc (wchar_t *, const char *, size_t);
int _mbtowc_r (struct _reent *, wchar_t *, const char *, size_t, _mbstate_t *);
int wctomb (char *, wchar_t);
int _wctomb_r (struct _reent *, char *, wchar_t, _mbstate_t *);
size_t mbstowcs (wchar_t *, const char *, size_t);
size_t _mbstowcs_r (struct _reent *, wchar_t *, const char *, size_t, _mbstate_t *);
size_t wcstombs (char *, const wchar_t *, size_t);
size_t _wcstombs_r (struct _reent *, char *, const wchar_t *, size_t, _mbstate_t *);
int mkstemp (char *);
char * mktemp (char *);
void qsort (void * __base, size_t __nmemb, size_t __size, int(*_compar)(const void *, const void *));
int rand (void);
void * realloc (void * __r, size_t __size);
void srand (unsigned __seed);
double strtod (const char *__n, char **__end_PTR);
double _strtod_r (struct _reent *,const char *__n, char **__end_PTR);
float strtof (const char *__n, char **__end_PTR);
long strtol (const char *__n, char **__end_PTR, int __base);
long _strtol_r (struct _reent *,const char *__n, char **__end_PTR, int __base);
unsigned long strtoul (const char *__n, char **__end_PTR, int __base);
unsigned long _strtoul_r (struct _reent *,const char *__n, char **__end_PTR, int __base);
int system (const char *__string);
long a64l (const char *__input);
char * l64a (long __input);
char * _l64a_r (struct _reent *,long __input);
int on_exit (void (*__func)(int, void *),void * __arg);
void _Exit (int __status);
int putenv (char *__string);
int _putenv_r (struct _reent *, char *__string);
int setenv (const char *__string, const char *__value, int __overwrite);
int _setenv_r (struct _reent *, const char *__string, const char *__value, int __overwrite);
char * gcvt (double,int,char *);
char * gcvtf (float,int,char *);
char * fcvt (double,int,int *,int *);
char * fcvtf (float,int,int *,int *);
char * ecvt (double,int,int *,int *);
char * ecvtbuf (double, int, int*, int*, char *);
char * fcvtbuf (double, int, int*, int*, char *);
char * ecvtf (float,int,int *,int *);
char * dtoa (double, int, int, int *, int*, char**);
int rand_r (unsigned *__seed);
double drand48 (void);
double _drand48_r (struct _reent *);
double erand48 (unsigned short [3]);
double _erand48_r (struct _reent *, unsigned short [3]);
long jrand48 (unsigned short [3]);
long _jrand48_r (struct _reent *, unsigned short [3]);
void lcong48 (unsigned short [7]);
void _lcong48_r (struct _reent *, unsigned short [7]);
long lrand48 (void);
long _lrand48_r (struct _reent *);
long mrand48 (void);
long _mrand48_r (struct _reent *);
long nrand48 (unsigned short [3]);
long _nrand48_r (struct _reent *, unsigned short [3]);
unsigned short *
       seed48 (unsigned short [3]);
unsigned short *
       _seed48_r (struct _reent *, unsigned short [3]);
void srand48 (long);
void _srand48_r (struct _reent *, long);
long long atoll (const char *__nptr);
long long _atoll_r (struct _reent *, const char *__nptr);
long long llabs (long long);
lldiv_t lldiv (long long __numer, long long __denom);
long long strtoll (const char *__n, char **__end_PTR, int __base);
long long _strtoll_r (struct _reent *, const char *__n, char **__end_PTR, int __base);
unsigned long long strtoull (const char *__n, char **__end_PTR, int __base);
unsigned long long _strtoull_r (struct _reent *, const char *__n, char **__end_PTR, int __base);
void cfree (void *);
void unsetenv (const char *__string);
void _unsetenv_r (struct _reent *, const char *__string);
char * _dtoa_r (struct _reent *, double, int, int, int *, int*, char**);
void * _malloc_r (struct _reent *, size_t);
void * _calloc_r (struct _reent *, size_t, size_t);
void _free_r (struct _reent *, void *);
void * _realloc_r (struct _reent *, void *, size_t);
void _mstats_r (struct _reent *, char *);
int _system_r (struct _reent *, const char *);
void __eprintf (const char *, const char *, unsigned int, const char *);
typedef char* __builtin_va_list;
typedef signed char __int8_t ;
typedef unsigned char __uint8_t ;
typedef signed short __int16_t;
typedef unsigned short __uint16_t;
typedef __int16_t __int_least16_t;
typedef __uint16_t __uint_least16_t;
typedef signed int __int32_t;
typedef unsigned int __uint32_t;
typedef __int32_t __int_least32_t;
typedef __uint32_t __uint_least32_t;
typedef signed long long __int64_t;
typedef unsigned long long __uint64_t;
typedef int ptrdiff_t;
typedef long int __off_t;
typedef int __pid_t;
typedef long long int __loff_t;
typedef unsigned char u_char;
typedef unsigned short u_short;
typedef unsigned int u_int;
typedef unsigned long u_long;
typedef unsigned short ushort; /* System V compatibility */
typedef unsigned int uint; /* System V compatibility */
typedef unsigned long clock_t;
typedef long time_t;
struct timespec {
  time_t tv_sec; /* Seconds */
  long tv_nsec; /* Nanoseconds */
};
struct itimerspec {
  struct timespec it_interval; /* Timer period */
  struct timespec it_value; /* Timer expiration */
};
typedef long daddr_t;
typedef char * caddr_t;
typedef unsigned short ino_t;
typedef short dev_t;
typedef long off_t;
typedef unsigned short uid_t;
typedef unsigned short gid_t;
typedef int pid_t;
typedef long key_t;
typedef _ssize_t ssize_t;
typedef unsigned int mode_t;
typedef unsigned short nlink_t;
typedef long fd_mask;
typedef struct _tag_types_fd_set {
 fd_mask fds_bits[(((64)+(((sizeof (fd_mask) * 8))-1))/((sizeof (fd_mask) * 8)))];
} _types_fd_set;
typedef unsigned long clockid_t;
typedef unsigned long timer_t;
typedef unsigned long useconds_t;
typedef long suseconds_t;
typedef __FILE FILE;
typedef _fpos_t fpos_t;
FILE * tmpfile (void);
char * tmpnam (char *);
int fclose (FILE *);
int fflush (FILE *);
FILE * freopen (const char *, const char *, FILE *);
void setbuf (FILE *, char *);
int setvbuf (FILE *, char *, int, size_t);
int fprintf (FILE *, const char *, ...);
int fscanf (FILE *, const char *, ...);
int printf (const char *, ...);
int scanf (const char *, ...);
int sscanf (const char *, const char *, ...);
int vfprintf (FILE *, const char *, ...);
int vprintf (const char *, ...);
int vsprintf (char *, const char *, ...);
int fgetc (FILE *);
char * fgets (char *, int, FILE *);
int fputc (int, FILE *);
int fputs (const char *, FILE *);
int getc (FILE *);
int getchar (void);
char * gets (char *);
int putc (int, FILE *);
int putchar (int);
int puts (const char *);
int ungetc (int, FILE *);
size_t fread (void *, size_t _size, size_t _n, FILE *);
size_t fwrite (const void * , size_t _size, size_t _n, FILE *);
int fgetpos (FILE *, fpos_t *);
int fseek (FILE *, long, int);
int fsetpos (FILE *, const fpos_t *);
long ftell ( FILE *);
void rewind (FILE *);
void clearerr (FILE *);
int feof (FILE *);
int ferror (FILE *);
void perror (const char *);
FILE * fopen (const char *_name, const char *_type);
int sprintf (char *, const char *, ...);
int remove (const char *);
int rename (const char *, const char *);
int fseeko (FILE *, off_t, int);
off_t ftello ( FILE *);
int asiprintf (char **, const char *, ...);
int asprintf (char **, const char *, ...);
int dprintf (int, const char *, ...);
int fcloseall (void);
int fiprintf (FILE *, const char *, ...);
int fiscanf (FILE *, const char *, ...);
int iprintf (const char *, ...);
int iscanf (const char *, ...);
int siprintf (char *, const char *, ...);
int siscanf (const char *, const char *, ...);
int snprintf (char *, size_t, const char *, ...);
int sniprintf (char *, size_t, const char *, ...);
char * tempnam (const char *, const char *);
int vasiprintf (char **, const char *, ...);
int vasprintf (char **, const char *, ...);
int vdprintf (int, const char *, ...);
int vsniprintf (char *, size_t, const char *, ...);
int vsnprintf (char *, size_t, const char *, ...);
int vfiprintf (FILE *, const char *, ...);
int vfiscanf (FILE *, const char *, ...);
int vfscanf (FILE *, const char *, ...);
int viprintf (const char *, ...);
int viscanf (const char *, ...);
int vscanf (const char *, ...);
int vsiscanf (const char *, const char *, ...);
int vsscanf (const char *, const char *, ...);
FILE * fdopen (int, const char *);
int fileno (FILE *);
int getw (FILE *);
int pclose (FILE *);
FILE * popen (const char *, const char *);
int putw (int, FILE *);
void setbuffer (FILE *, char *, int);
int setlinebuf (FILE *);
int getc_unlocked (FILE *);
int getchar_unlocked (void);
void flockfile (FILE *);
int ftrylockfile (FILE *);
void funlockfile (FILE *);
int putc_unlocked (int, FILE *);
int putchar_unlocked (int);
int _asiprintf_r (struct _reent *, char **, const char *, ...);
int _asprintf_r (struct _reent *, char **, const char *, ...);
int _dprintf_r (struct _reent *, int, const char *, ...);
int _fcloseall_r (struct _reent *);
FILE * _fdopen_r (struct _reent *, int, const char *);
FILE * _fopen_r (struct _reent *, const char *, const char *);
int _fclose_r (struct _reent *, FILE *);
char * _fgets_r (struct _reent *, char *, int, FILE *);
int _fiscanf_r (struct _reent *, FILE *, const char *, ...);
int _fputc_r (struct _reent *, int, FILE *);
int _fputs_r (struct _reent *, const char *, FILE *);
size_t _fread_r (struct _reent *, void *, size_t _size, size_t _n, FILE *);
int _fscanf_r (struct _reent *, FILE *, const char *, ...);
int _fseek_r (struct _reent *, FILE *, long, int);
long _ftell_r (struct _reent *, FILE *);
size_t _fwrite_r (struct _reent *, const void * , size_t _size, size_t _n, FILE *);
int _getc_r (struct _reent *, FILE *);
int _getc_unlocked_r (struct _reent *, FILE *);
int _getchar_r (struct _reent *);
int _getchar_unlocked_r (struct _reent *);
char * _gets_r (struct _reent *, char *);
int _iprintf_r (struct _reent *, const char *, ...);
int _iscanf_r (struct _reent *, const char *, ...);
int _mkstemp_r (struct _reent *, char *);
char * _mktemp_r (struct _reent *, char *);
void _perror_r (struct _reent *, const char *);
int _printf_r (struct _reent *, const char *, ...);
int _putc_r (struct _reent *, int, FILE *);
int _putc_unlocked_r (struct _reent *, int, FILE *);
int _putchar_unlocked_r (struct _reent *, int);
int _putchar_r (struct _reent *, int);
int _puts_r (struct _reent *, const char *);
int _remove_r (struct _reent *, const char *);
int _rename_r (struct _reent *, const char *_old, const char *_new);
int _scanf_r (struct _reent *, const char *, ...);
int _siprintf_r (struct _reent *, char *, const char *, ...);
int _siscanf_r (struct _reent *, const char *, const char *, ...);
int _sniprintf_r (struct _reent *, char *, size_t, const char *, ...);
int _snprintf_r (struct _reent *, char *, size_t, const char *, ...);
int _sprintf_r (struct _reent *, char *, const char *, ...);
int _sscanf_r (struct _reent *, const char *, const char *, ...);
char * _tempnam_r (struct _reent *, const char *, const char *);
FILE * _tmpfile_r (struct _reent *);
char * _tmpnam_r (struct _reent *, char *);
int _ungetc_r (struct _reent *, int, FILE *);
int _vasiprintf_r (struct _reent *, char **, const char *, ...);
int _vasprintf_r (struct _reent *, char **, const char *, ...);
int _vdprintf_r (struct _reent *, int, const char *, ...);
int _vfiprintf_r (struct _reent *, FILE *, const char *, ...);
int _vfprintf_r (struct _reent *, FILE *, const char *, ...);
int _viprintf_r (struct _reent *, const char *, ...);
int _vprintf_r (struct _reent *, const char *, ...);
int _vsiprintf_r (struct _reent *, char *, const char *, ...);
int _vsprintf_r (struct _reent *, char *, const char *, ...);
int _vsniprintf_r (struct _reent *, char *, size_t, const char *, ...);
int _vsnprintf_r (struct _reent *, char *, size_t, const char *, ...);
int _vfiscanf_r (struct _reent *, FILE *, const char *, ...);
int _vfscanf_r (struct _reent *, FILE *, const char *, ...);
int _viscanf_r (struct _reent *, const char *, ...);
int _vscanf_r (struct _reent *, const char *, ...);
int _vsscanf_r (struct _reent *, const char *, const char *, ...);
int _vsiscanf_r (struct _reent *, const char *, const char *, ...);
ssize_t __getdelim (char **, size_t *, int, FILE *);
ssize_t __getline (char **, size_t *, FILE *);
int __srget_r (struct _reent *, FILE *);
int __swbuf_r (struct _reent *, int, FILE *);
FILE *funopen (const void * _cookie, int (*readfn)(void * _cookie, char *_buf, int _n), int (*writefn)(void * _cookie, const char *_buf, int _n), fpos_t (*seekfn)(void * _cookie, fpos_t _off, int _whence), int (*closefn)(void * _cookie));
typedef enum {
  FALSE,
  TRUE
} Boolean;
typedef struct
{
  unsigned cpb_cnt; // ue(v)
  unsigned bit_rate_scale; // u(4)
  unsigned cpb_size_scale; // u(4)
    unsigned bit_rate_value [32]; // ue(v)
    unsigned cpb_size_value[32]; // ue(v)
    unsigned vbr_cbr_flag[32]; // u(1)
  unsigned initial_cpb_removal_delay_length_minus1; // u(5)
  unsigned cpb_removal_delay_length_minus1; // u(5)
  unsigned dpb_output_delay_length_minus1; // u(5)
  unsigned time_offset_length; // u(5)
} hrd_parameters_t;
typedef struct
{
  Boolean aspect_ratio_info_present_flag; // u(1)
    unsigned aspect_ratio_idc; // u(8)
      unsigned sar_width; // u(16)
      unsigned sar_height; // u(16)
  Boolean overscan_info_present_flag; // u(1)
    Boolean overscan_appropriate_flag; // u(1)
  Boolean video_signal_type_present_flag; // u(1)
    unsigned video_format; // u(3)
    Boolean video_full_range_flag; // u(1)
    Boolean colour_description_present_flag; // u(1)
      unsigned colour_primaries; // u(8)
      unsigned transfer_characteristics; // u(8)
      unsigned matrix_coefficients; // u(8)
  Boolean chroma_location_info_present_flag; // u(1)
    unsigned chroma_location_frame; // ue(v)
    unsigned chroma_location_field; // ue(v)
  Boolean timing_info_present_flag; // u(1)
    unsigned num_units_in_tick; // u(32)
    unsigned time_scale; // u(32)
    Boolean fixed_frame_rate_flag; // u(1)
  Boolean nal_hrd_parameters_present_flag; // u(1)
    hrd_parameters_t nal_hrd_parameters; // hrd_paramters_t
  Boolean vcl_hrd_parameters_present_flag; // u(1)
    hrd_parameters_t vcl_hrd_parameters; // hrd_paramters_t
  // if ((nal_hrd_parameters_present_flag || (vcl_hrd_parameters_present_flag))
    Boolean low_delay_hrd_flag; // u(1)
  Boolean bitstream_restriction_flag; // u(1)
    Boolean motion_vectors_over_pic_boundaries_flag; // u(1)
    unsigned max_bytes_per_pic_denom; // ue(v)
    unsigned max_bits_per_mb_denom; // ue(v)
    unsigned log2_max_mv_length_vertical; // ue(v)
    unsigned log2_max_mv_length_horizontal; // ue(v)
    unsigned max_dec_frame_reordering; // ue(v)
    unsigned max_dec_frame_buffering; // ue(v)
} vui_seq_parameters_t;
typedef struct
{
  Boolean Valid; // indicates the parameter set is valid
  unsigned pic_parameter_set_id; // ue(v)
  unsigned seq_parameter_set_id; // ue(v)
  Boolean entropy_coding_mode_flag; // u(1)
  // if( pic_order_cnt_type < 2 )  in the sequence parameter set
  Boolean pic_order_present_flag; // u(1)
  unsigned num_slice_groups_minus1; // ue(v)
    unsigned slice_group_map_type; // ue(v)
    // if( slice_group_map_type = = 0 )
      unsigned run_length_minus1[8]; // ue(v)
    // else if( slice_group_map_type = = 2 )
      unsigned top_left[8]; // ue(v)
      unsigned bottom_right[8]; // ue(v)
    // else if( slice_group_map_type = = 3 || 4 || 5
      Boolean slice_group_change_direction_flag; // u(1)
      unsigned slice_group_change_rate_minus1; // ue(v)
    // else if( slice_group_map_type = = 6 )
      unsigned num_slice_group_map_units_minus1; // ue(v)
      unsigned *slice_group_id; // complete MBAmap u(v)
  unsigned num_ref_idx_l0_active_minus1; // ue(v)
  unsigned num_ref_idx_l1_active_minus1; // ue(v)
  Boolean weighted_pred_flag; // u(1)
  Boolean weighted_bipred_idc; // u(2)
  int pic_init_qp_minus26; // se(v)
  int pic_init_qs_minus26; // se(v)
  int chroma_qp_index_offset; // se(v)
  Boolean deblocking_filter_control_present_flag; // u(1)
  Boolean constrained_intra_pred_flag; // u(1)
  Boolean redundant_pic_cnt_present_flag; // u(1)
  Boolean vui_pic_parameters_flag; // u(1)
} pic_parameter_set_rbsp_t;
typedef struct
{
  Boolean Valid; // indicates the parameter set is valid
  unsigned profile_idc; // u(8)
  Boolean constrained_set0_flag; // u(1)
  Boolean constrained_set1_flag; // u(1)
  Boolean constrained_set2_flag; // u(1)
  unsigned level_idc; // u(8)
  unsigned seq_parameter_set_id; // ue(v)
  unsigned log2_max_frame_num_minus4; // ue(v)
  unsigned pic_order_cnt_type;
  // if( pic_order_cnt_type == 0 )
  unsigned log2_max_pic_order_cnt_lsb_minus4; // ue(v)
  // else if( pic_order_cnt_type == 1 )
    Boolean delta_pic_order_always_zero_flag; // u(1)
    int offset_for_non_ref_pic; // se(v)
    int offset_for_top_to_bottom_field; // se(v)
    unsigned num_ref_frames_in_pic_order_cnt_cycle; // ue(v)
    // for( i = 0; i < num_ref_frames_in_pic_order_cnt_cycle; i++ )
      int offset_for_ref_frame[255]; // se(v)
  unsigned num_ref_frames; // ue(v)
  Boolean gaps_in_frame_num_value_allowed_flag; // u(1)
  unsigned pic_width_in_mbs_minus1; // ue(v)
  unsigned pic_height_in_map_units_minus1; // ue(v)
  Boolean frame_mbs_only_flag; // u(1)
  // if( !frame_mbs_only_flag )
    Boolean mb_adaptive_frame_field_flag; // u(1)
  Boolean direct_8x8_inference_flag; // u(1)
  Boolean frame_cropping_flag; // u(1)
    unsigned frame_cropping_rect_left_offset; // ue(v)
    unsigned frame_cropping_rect_right_offset; // ue(v)
    unsigned frame_cropping_rect_top_offset; // ue(v)
    unsigned frame_cropping_rect_bottom_offset; // ue(v)
  Boolean vui_parameters_present_flag; // u(1)
    vui_seq_parameters_t vui_seq_parameters; // vui_seq_parameters_t
} seq_parameter_set_rbsp_t;
pic_parameter_set_rbsp_t *AllocPPS ();
seq_parameter_set_rbsp_t *AllocSPS ();
void FreePPS (pic_parameter_set_rbsp_t *pps);
void FreeSPS (seq_parameter_set_rbsp_t *sps);
typedef unsigned char byte; //!<  8 bit unsigned
typedef int int32;
typedef unsigned int u_int32;
pic_parameter_set_rbsp_t *active_pps;
seq_parameter_set_rbsp_t *active_sps;
// global picture format dependend buffers, mem allocation in decod.c ******************
int **refFrArr; //!< Array for reference frames of each block
byte **imgY_ref; //!< reference frame find snr
byte ***imgUV_ref;
int ReMapRef[20];
// B pictures
int Bframe_ctr;
int frame_no;
// For MB level frame/field coding
int TopFieldForSkip_Y[16][16];
int TopFieldForSkip_UV[2][16][16];
char errortext[300]; //!< buffer for error message for exit with error()
int g_new_frame;
typedef enum
{
  PAR_DP_1, //<! no data partitioning is supported
  PAR_DP_3, //<! data partitioning with 3 partitions
} PAR_DP_TYPE;
typedef enum
{
  PAR_OF_ANNEXB, //<! Current TML description
  PAR_OF_RTP, //<! RTP Packet Output format
//  PAR_OF_IFF    //<! Interim File Format
} PAR_OF_TYPE;
typedef enum {
  SE_HEADER,
  SE_PTYPE,
  SE_MBTYPE,
  SE_REFFRAME,
  SE_INTRAPREDMODE,
  SE_MVD,
  SE_CBP_INTRA,
  SE_LUM_DC_INTRA,
  SE_CHR_DC_INTRA,
  SE_LUM_AC_INTRA,
  SE_CHR_AC_INTRA,
  SE_CBP_INTER,
  SE_LUM_DC_INTER,
  SE_CHR_DC_INTER,
  SE_LUM_AC_INTER,
  SE_CHR_AC_INTER,
  SE_DELTA_QUANT_INTER,
  SE_DELTA_QUANT_INTRA,
  SE_BFRAME,
  SE_EOS,
  SE_MAX_ELEMENTS //!< number of maximum syntax elements, this MUST be the last one!
} SE_type; // substituting the definitions in element.h
typedef enum {
  INTER_MB,
  INTRA_MB_4x4,
  INTRA_MB_16x16
} IntraInterDecision;
typedef enum {
  BITS_TOTAL_MB,
  BITS_HEADER_MB,
  BITS_INTER_MB,
  BITS_CBP_MB,
  BITS_COEFF_Y_MB,
  BITS_COEFF_UV_MB,
  MAX_BITCOUNTER_MB
} BitCountType;
typedef enum {
  NO_SLICES,
  FIXED_MB,
  FIXED_RATE,
  CALLBACK,
  FMO
} SliceMode;
typedef enum {
  UVLC,
  CABAC
} SymbolMode;
typedef enum {
  FRAME,
  TOP_FIELD,
  BOTTOM_FIELD
} PictureStructure; //!< New enum for field processing
typedef enum {
  P_SLICE = 0,
  B_SLICE,
  I_SLICE,
  SP_SLICE,
  SI_SLICE
} SliceType;
/***********************************************************************
 * D a t a    t y p e s   f o r  C A B A C
 ***********************************************************************
 */
//! struct to characterize the state of the arithmetic coding engine
typedef struct
{
  unsigned int Dlow, Drange;
  unsigned int Dvalue;
  unsigned int Dbuffer;
  int Dbits_to_go;
  byte *Dcodestrm;
  int *Dcodestrm_len;
} DecodingEnvironment;
typedef DecodingEnvironment *DecodingEnvironmentPtr;
typedef struct
{
  unsigned short state; // index into state-table CP
  unsigned char MPS; // Least Probable Symbol 0/1 CP
} BiContextType;
typedef BiContextType *BiContextTypePtr;
typedef struct
{
  BiContextType mb_type_contexts [4][11];
  BiContextType b8_type_contexts [2][9];
  BiContextType mv_res_contexts [2][10];
  BiContextType ref_no_contexts [2][6];
  BiContextType delta_qp_contexts[4];
  BiContextType mb_aff_contexts [4];
} MotionInfoContexts;
typedef struct
{
  BiContextType ipr_contexts [2];
  BiContextType cipr_contexts[4];
  BiContextType cbp_contexts [3][4];
  BiContextType bcbp_contexts[8][4];
  BiContextType map_contexts [8][15];
  BiContextType last_contexts[8][15];
  BiContextType one_contexts [8][5];
  BiContextType abs_contexts [8][5];
  BiContextType fld_map_contexts [8][15];
  BiContextType fld_last_contexts[8][15];
} TextureInfoContexts;
struct img_par;
struct inp_par;
struct stat_par;
typedef struct DecRefPicMarking_s
{
  int memory_management_control_operation;
  int difference_of_pic_nums_minus1;
  int long_term_pic_num;
  int long_term_frame_idx;
  int max_long_term_frame_idx_plus1;
  struct DecRefPicMarking_s *Next;
} DecRefPicMarking_t;
typedef struct syntaxelement
{
  int type; //!< type of syntax element for data part.
  int value1; //!< numerical value of syntax element
  int value2; //!< for blocked symbols, e.g. run/level
  int len; //!< length of code
  int inf; //!< info part of UVLC code
  unsigned int bitpattern; //!< UVLC bitpattern
  int context; //!< CABAC context
  int k; //!< CABAC context for coeff_count,uv
  //! for mapping of UVLC to syntaxElement
  void (*mapping)(int len, int info, int *value1, int *value2);
  //! used for CABAC: refers to actual coding method of each individual syntax element type
  void (*reading)(struct syntaxelement *, struct inp_par *, struct img_par *, DecodingEnvironmentPtr);
} SyntaxElement;
//! Macroblock
typedef struct macroblock
{
  int qp;
  int slice_nr;
  int delta_quant; //!< for rate control
  struct macroblock *mb_available_up; //!< pointer to neighboring MB (CABAC)
  struct macroblock *mb_available_left; //!< pointer to neighboring MB (CABAC)
  // some storage of macroblock syntax elements for global access
  int mb_type;
  int mvd[2][(16/4)][(16/4)][2]; //!< indices correspond to [forw,backw][block_y][block_x][x,y]
  int cbp, cbp_blk ;
  unsigned long cbp_bits;
  int is_skip;
  int i16mode;
  int b8mode[4];
  int b8pdir[4];
  int ei_flag;
  int LFDisableIdc;
  int LFAlphaC0Offset;
  int LFBetaOffset;
  int c_ipred_mode; //!< chroma intra prediction mode
  int mb_field;
  int mbAddrA, mbAddrB, mbAddrC, mbAddrD;
  int mbAvailA, mbAvailB, mbAvailC, mbAvailD;
} Macroblock;
//! Bitstream
typedef struct
{
  // CABAC Decoding
  int read_len; //!< actual position in the codebuffer, CABAC only
  int code_len; //!< overall codebuffer length, CABAC only
  // UVLC Decoding
  int frame_bitoffset; //!< actual position in the codebuffer, bit-oriented, UVLC only
  int bitstream_length; //!< over codebuffer lnegth, byte oriented, UVLC only
  // ErrorConcealment
  byte *streamBuffer; //!< actual codebuffer for read bytes
  int ei_flag; //!< error indication, 0: no error, else unspecified error
} Bitstream;
//! DataPartition
typedef struct datapartition
{
  Bitstream *bitstream;
  DecodingEnvironment de_cabac;
  int (*readSyntaxElement)(SyntaxElement *, struct img_par *, struct inp_par *, struct datapartition *);
          /*!< virtual function;
               actual method depends on chosen data partition and
               entropy coding method  */
} DataPartition;
//! Slice
typedef struct
{
  int ei_flag; //!< 0 if the partArr[0] contains valid information
  int qp;
  int picture_type; //!< picture type
  PictureStructure structure; //!< Identify picture structure type
  int start_mb_nr; //!< MUST be set by NAL even in case of ei_flag == 1
  int max_part_nr;
  int dp_mode; //!< data partioning mode
  int next_header;
//  int                 last_mb_nr;    //!< only valid when entropy coding == CABAC
  DataPartition *partArr; //!< array of partitions
  MotionInfoContexts *mot_ctx; //!< pointer to struct of context models for use in CABAC
  TextureInfoContexts *tex_ctx; //!< pointer to struct of context models for use in CABAC
  int ref_pic_list_reordering_flag_l0;
  int *remapping_of_pic_nums_idc_l0;
  int *abs_diff_pic_num_minus1_l0;
  int *long_term_pic_idx_l0;
  int ref_pic_list_reordering_flag_l1;
  int *remapping_of_pic_nums_idc_l1;
  int *abs_diff_pic_num_minus1_l1;
  int *long_term_pic_idx_l1;
  int (*readSlice)(struct img_par *, struct inp_par *);
  int LFDisableIdc; //!< Disable loop filter on slice
  int LFAlphaC0Offset; //!< Alpha and C0 offset for filtering slice
  int LFBetaOffset; //!< Beta offset for filtering slice
  int pic_parameter_set_id; //!<the ID of the picture parameter set the slice is reffering to
} Slice;
//****************************** ~DM ***********************************
// image parameters
typedef struct img_par
{
  int number; //<! frame number
  unsigned current_mb_nr; // bitstream order
} ImageParameters;
extern ImageParameters *img;
extern struct snr_par *snr;
// signal to noice ratio parameters
struct snr_par
{
  float snr_y; //<! current Y SNR
  float snr_u; //<! current U SNR
  float snr_v; //<! current V SNR
  float snr_y1; //<! SNR Y(dB) first frame
  float snr_u1; //<! SNR U(dB) first frame
  float snr_v1; //<! SNR V(dB) first frame
  float snr_ya; //<! Average SNR Y(dB) remaining frames
  float snr_ua; //<! Average SNR U(dB) remaining frames
  float snr_va; //<! Average SNR V(dB) remaining frames
};
int tot_time;
// input parameters from configuration file
struct inp_par
{
  char infile[100]; //<! Telenor H.26L input
  char outfile[100]; //<! Decoded YUV 4:2:0 output
  char reffile[100]; //<! Optional YUV 4:2:0 reference file for SNR measurement
  int FileFormat; //<! File format of the Input file, PAR_OF_ANNEXB or PAR_OF_RTP
  int dpb_size; //<! Frame buffer size
};
extern struct inp_par *input;
typedef struct pix_pos
{
  int available;
  int mb_addr;
  int x;
  int y;
  int pos_x;
  int pos_y;
} PixelPos;
// files
FILE *p_out; //<! pointer to output YUV file
//FILE *p_out2;                    //<! pointer to debug output YUV file
FILE *p_ref; //<! pointer to input original reference YUV file file
FILE *p_log; //<! SNR file
// prototypes
void init_conf(struct inp_par *inp, char *config_filename);
void report(struct inp_par *inp, struct img_par *img, struct snr_par *snr);
void init(struct img_par *img);
void malloc_slice(struct inp_par *inp, struct img_par *img);
void free_slice(struct inp_par *inp, struct img_par *img);
int decode_one_frame(struct img_par *img,struct inp_par *inp, struct snr_par *snr);
void init_frame(struct img_par *img, struct inp_par *inp);
void exit_frame(struct img_par *img, struct inp_par *inp);
void DeblockFrame(struct img_par *img, byte **imgY, byte ***imgUV ) ;
int read_new_slice();
void decode_one_slice(struct img_par *img,struct inp_par *inp);
void start_macroblock(struct img_par *img,struct inp_par *inp, int CurrentMBInScanOrder);
int read_one_macroblock(struct img_par *img,struct inp_par *inp);
void read_ipred_modes(struct img_par *img,struct inp_par *inp);
int decode_one_macroblock(struct img_par *img,struct inp_par *inp);
int exit_macroblock(struct img_par *img,struct inp_par *inp, int eos_bit);
void readMotionInfoFromNAL (struct img_par *img,struct inp_par *inp);
void readCBPandCoeffsFromNAL(struct img_par *img,struct inp_par *inp);
void copyblock_sp(struct img_par *img,int block_x,int block_y);
void itrans_sp_chroma(struct img_par *img,int ll);
void itrans(struct img_par *img,int ioff,int joff,int i0,int j0);
void itrans_sp(struct img_par *img,int ioff,int joff,int i0,int j0);
int intrapred(struct img_par *img,int ioff,int joff,int i4,int j4);
void itrans_2(struct img_par *img);
int intrapred_luma_16x16(struct img_par *img,int predmode);
void intrapred_chroma(struct img_par *img, int uv);
int sign(int a , int b);
int (*nal_startcode_follows) ();
int uvlc_startcode_follows();
int cabac_startcode_follows();
void free_Partition(Bitstream *currStream);
void reset_ec_flags();
void error(char *text, int code);
int init_global_buffers(struct inp_par *inp, struct img_par *img);
void free_global_buffers(struct inp_par *inp, struct img_par *img);
void frame_postprocessing(struct img_par *img, struct inp_par *inp);
void field_postprocessing(struct img_par *img, struct inp_par *inp);
int bottom_field_picture(struct img_par *img,struct inp_par *inp);
void init_top(struct img_par *img, struct inp_par *inp);
void init_bottom(struct img_par *img, struct inp_par *inp);
void decode_frame_slice(struct img_par *img,struct inp_par *inp, int current_header);
void decode_field_slice(struct img_par *img,struct inp_par *inp, int current_header);
int RBSPtoSODB(byte *streamBuffer, int last_byte_pos);
int EBSPtoRBSP(byte *streamBuffer, int end_bytepos, int begin_bytepos);
void init_super_macroblock(struct img_par *img,struct inp_par *inp);
void exit_super_macroblock(struct img_par *img,struct inp_par *inp);
int decode_super_macroblock(struct img_par *img,struct inp_par *inp);
void decode_one_Copy_topMB(struct img_par *img,struct inp_par *inp);
void SetOneRefMV(struct img_par* img);
int peekSyntaxElement_UVLC(SyntaxElement *sym, struct img_par *img, struct inp_par *inp, struct datapartition *dP);
void fill_wp_params(struct img_par *img);
void reset_wp_params(struct img_par *img);
void FreePartition (DataPartition *dp, int n);
DataPartition *AllocPartition();
void tracebits2(const char *trace_str, int len, int info);
extern const byte QP_SCALE_CR[52] ;
extern const int dequant_coef[6][4][4];
typedef struct storable_picture
{
  PictureStructure structure;
  int poc;
  int top_poc;
  int bottom_poc;
  int order_num;
  int ref_pic_num[6][20];
  int pic_num;
  int long_term_pic_num;
  int long_term_frame_idx;
  int is_long_term;
  int used_for_reference;
  int size_x, size_y, size_x_cr, size_y_cr;
  int chroma_vector_adjustment;
  int coded_frame;
  int mb_adaptive_frame_field_flag;
  byte ** imgY;
  byte *** imgUV;
  byte * mb_field; //<! field macroblock indicator
  int *** ref_idx; //<! reference picture   [list][subblock_x][subblock_y]
                             //   [list][mb_nr][subblock_x][subblock_y]
  int *** ref_pic_id; //<! reference picture identifier [list][subblock_x][subblock_y]
                             //   (not  simply index)
  int **** mv; //<! motion vector       [list][subblock_x][subblock_y][component]
  byte ** moving_block;
  struct storable_picture *top_field; // for mb aff, if frame for referencing the top field
  struct storable_picture *bottom_field; // for mb aff, if frame for referencing the bottom field
  struct storable_picture *frame; // for mb aff, if field for referencing the combined frame
} StorablePicture;
//! Frame Stores for Decoded Picture Buffer
typedef struct frame_store
{
  int is_used; //<! 0=empty; 1=top; 2=bottom; 3=both fields (or frame)
  int is_reference; //<! 0=not used for ref; 1=top used; 2=bottom used; 3=both fields (or frame) used
  int is_long_term; //<! 0=not used for ref; 1=top used; 2=bottom used; 3=both fields (or frame) used
  int is_non_existent;
  unsigned frame_num;
  int frame_num_wrap;
  int long_term_frame_idx;
  int is_output;
  int poc;
  StorablePicture *frame;
  StorablePicture *top_field;
  StorablePicture *bottom_field;
} FrameStore;
//! Decoded Picture Buffer
typedef struct decoded_picture_buffer
{
  FrameStore **fs;
  FrameStore **fs_ref;
  FrameStore **fs_ltref;
  unsigned size;
  unsigned used_size;
  unsigned ref_frames_in_buffer;
  unsigned ltref_frames_in_buffer;
  int last_output_poc;
  int max_long_term_pic_idx;
} DecodedPictureBuffer;
extern DecodedPictureBuffer dpb;
extern StorablePicture **listX[6];
extern int listXsize[6];
void init_dpb(struct inp_par *inp);
void free_dpb();
FrameStore* alloc_frame_store();
void free_frame_store(FrameStore* f);
StorablePicture* alloc_storable_picture(PictureStructure type, int size_x, int size_y, int size_x_cr, int size_y_cr);
void free_storable_picture(StorablePicture* p);
void store_picture_in_dpb(StorablePicture* p);
void flush_dpb();
void dpb_split_field(FrameStore *fs);
void dpb_combine_field(FrameStore *fs);
void init_lists(int currSliceType, PictureStructure currPicStructure);
void reorder_ref_pic_list(StorablePicture **list, int *list_size,
                                      int num_ref_idx_lX_active_minus1, int *remapping_of_pic_nums_idc,
                                      int *abs_diff_pic_num_minus1, int *long_term_pic_idx);
void init_mbaff_lists();
void alloc_ref_pic_list_reordering_buffer(Slice *currSlice);
void free_ref_pic_list_reordering_buffer(Slice *currSlice);
extern StorablePicture *dec_picture;
void find_snr(struct snr_par *snr, StorablePicture *p, FILE *p_ref);
void get_block(int ref_frame, StorablePicture **list, int x_pos, int y_pos, struct img_par *img, int block[4][4]);
int picture_order(struct img_par *img);
void CheckAvailabilityOfNeighbors();
void getNeighbour(int curr_mb_nr, int xN, int yN, int luma, PixelPos *pix);
void getLuma4x4Neighbour (int curr_mb_nr, int block_x, int block_y, int rel_x, int rel_y, PixelPos *pix);
void getChroma4x4Neighbour (int curr_mb_nr, int block_x, int block_y, int rel_x, int rel_y, PixelPos *pix);
int mb_is_available(int mbAddr, int currMbAddr);
void get_mb_pos (int mb_addr, int *x, int*y);
void get_mb_block_pos (int mb_addr, int *x, int*y);
int intrapred(struct img_par *img, int ioff, int joff)
{
  int PredPel[10]; // array of predictor pels
  byte ** imgY = dec_picture->imgY;
  PixelPos pix_b, pix_c;
  int mb_nr=img->current_mb_nr;
  getNeighbour(ioff, ioff , joff -1 , 1, &pix_b);
  getNeighbour(ioff, ioff +4 , joff -1 , 1, &pix_c);
  PredPel[4] = imgY[joff][3];
  return 0;
}
