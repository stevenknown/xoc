// This file contains a recursive descent parser for C.
//
// Most functions in this file are named after the symbols they are
// supposed to read from an input token list. For example, stmt() is
// responsible for reading a statement from a token list. The function
// then construct an AST node representing a statement.
//
// Each function conceptually returns two values, an AST node and
// remaining part of the input tokens. Since C doesn't support
// multiple return values, the remaining tokens are returned to the
// caller via a pointer argument.
//
// Input tokens are represented by a linked list. Unlike many recursive
// descent parsers, we don't have the notion of the "input token stream".
// Most parsing functions don't change the global state of the parser.
// So it is very easy to lookahead arbitrary number of tokens in this
// parser.





void abort(); 
void assert(char v);


 




int ispunct(int c);
int isxdigit(int c);
int isspace(int c);
int isalnum(int c);
int isdigit(int c);
int isprint(int c);
int isalpha(int c);
int isgraph(int c);






extern int errno;





typedef long unsigned int size_t;



    







char *dirname(const char *path);
char *basename(const char *path);






/* The macro __STDC__ is defined by all versions of the ANSI C compiler.

   The macro __STDC_VERSION__ is defined by ANSI C compilers that
   support the C99 standard.  It is not defined by pre-C99 versions.

   The macro __STDC_HOSTED__ is defined by all ANSI C implementations
   that are not hosted implementations (i.e., they are freestanding).

   The macro __STDC_NO_VLAs__ is defined by implementations that do not
   support C99 variable-length arrays, whether or not they are otherwise
   C99 compliant.  */





/* All the headers included by stdarg.h itself.  */
//#include <stdarg.h>

/* Define __va_list_tag to indicate that the file is being used properly.  */


/* The macro for declaring a variable of type `va_list'.  */


/* The macro for starting the use of the variable `ap'.  */


/* The macro for ending the use of the variable `ap'.  */


/* The macro for getting the next argument of type `type'.  */


/* The macro for copying the `next argument' state from `from' to `to'.  */









/* All the definitions below are implementation-specific.  */

/* Thestdbool.h header shall define _Bool and the constants true and
   false.  If the implementation provides the stdbool.h header, it
   shall define these macros in the std namespace.  */

typedef int bool;









//#include <limits.h>

/* Exact-width integer types.  */
typedef signed char        int8_t;
typedef unsigned char       uint8_t;

typedef signed short       int16_t;
typedef unsigned short      uint16_t;

typedef signed int          int32_t;
typedef unsigned int        uint32_t;

typedef signed long long   int64_t;
typedef unsigned long long uint64_t;

typedef signed int intptr_t;

/* Minimum and maximum values for these types.  */


















///* Limits of integral types.  */
//#if defined(__INT_MAX__) && defined(__LONG_MAX__)
//  #define INTMAX_MIN (-__LONG_MAX__ - 1)
//  #define INTMAX_MAX  __LONG_MAX__
//  #define UINTMAX_MAX __LONG_MAX__
//#elif defined(__INT64_MAX__)
//  #define INTMAX_MIN (-__INT64_MAX__ - 1)
//  #define INTMAX_MAX  __INT64_MAX__
//  #define UINTMAX_MAX __UINT64_MAX
//#else
//  #error "No integral type is large enough to be INTMAX"
//#endif

/* Limits of the common real types.  */
//#define FLOAT_MIN      (1.17549435e-38F)
//#define FLOAT_MAX      (3.40282347e+38F)
//#define DOUBLE_MIN     (2.22507287e-308)
//#define DOUBLE_MAX     (1.79769e+308)
//#define LONG_DOUBLE_MIN (3.36210314311209350e-4932L)
//#define LONG_DOUBLE_MAX (1.18973149535723176502e+4932L)

/* Limits of other real types.  */
//#define LDBL_MIN      (3.36210314311209350e-4932L)
//#define LDBL_MAX      (1.18973149535723176502e+4932L)

/* Least significant bit, etc.  */










/* Compile-time checks for minimum-width integer types.  */
//_Static_assert(sizeof(int8_t) == 1, "int8_t is not 8 bits");
//_Static_assert(sizeof(uint8_t) == 1, "uint8_t is not 8 bits");
//_Static_assert(sizeof(int16_t) == 2, "int16_t is not 16 bits");
//_Static_assert(sizeof(uint16_t) == 2, "uint16_t is not 16 bits");
//_Static_assert(sizeof(int32_t) == 4, "int32_t is not 32 bits");
//_Static_assert(sizeof(uint32_t) == 4, "uint32_t is not 32 bits");
//_Static_assert(sizeof(int64_t) == 8, "int64_t is not 64 bits");
//_Static_assert(sizeof(uint64_t) == 8, "uint64_t is not 64 bits");

/* stdint.h */





int atoi(const char *str);







void * calloc(int);
extern void abort();
extern void exit(int);
void * malloc(int);
void free(void*);
unsigned long strtoul(const char *str, char **endptr, int base);
double strtold(const char *str, char **endptr);
void *realloc(void *ptr, size_t new_size);
int mkstemp(char *template);
void _exit(int status);
int atexit(void (*func)(void));
void *calloc(size_t num, size_t size);
void qsort(void *base, size_t num, size_t size,
           int (*compar)(const void *, const void *));
int mkstemps(char *template, int suffixlen);
void perror(const char *s);









/* Seek method constants */






typedef struct FILE FILE;
struct FILE;
extern int stderr;
extern int stdin;
extern int stdout;


void printf(char const*,...);
void fprintf(FILE *, char const*, ...);
int vfprintf(FILE *stream, const char *format, unsigned char*  args);
int vprintf(const char *format, unsigned char*  arg);
int sprintf(char *str, const char *format, ...);
int vsnprintf(char *str, size_t size, const char *format, unsigned char*  args);
FILE * fopen(char const*, char const*);
void fflush(FILE *);
void fclose(FILE *);
int ferror(FILE *);
char * fgets(char * s, int size, FILE * stream);
size_t fread( void *buffer, size_t size, size_t count, FILE *stream );
size_t fwrite(void *ptr, size_t size, size_t nmemb, FILE *stream);
int fseek(FILE *stream, long int offset, int whence);
FILE *open_memstream(char **ptr, size_t *sizeloc);
int fputc(int c, FILE *stream);
int unlink(const char *pathname);
int fileno(FILE *stream);
int getc(FILE *stream);
int fgetc(FILE *stream);
int feof(FILE *stream);
int ungetc(int c, FILE *stream);
void setbuf(FILE *stream, char *buf);
char *getcwd(char *buf, size_t size);








//#include "../include/stdnoreturn.h"





int strcpy(char*, char const*);
int strcmp(char*, char const*);
int strncmp(const char *s1, const char *s2, size_t n);
char *strdup(const char *s);
char *strndup(const char *s, size_t n);
char *strerror(int errnum);
char *strrchr(const char *str, int c);
char *strncpy(char *dest, const char *src, size_t n);

//Note strncasecmp is not the C99 standard api.
int strncasecmp(const char *s1, const char *s2, size_t n);
char *strchr(const char *haystack, int needle);
char *strstr(const char *haystack, const char *needle);
int strlen(char const*);
void * memcpy(void *dest, void *src, size_t count);
int memcmp(const void *str1, const void *str2, size_t n);
void *memset(void *s, int ch, size_t count);
size_t strcspn(const char *str1, const char *str2);
char *strtok(char *restrict s, const char *restrict delimiters);



//#include "../include/strings.h"





typedef unsigned int pid_t;
typedef unsigned long __dev_t;
typedef __dev_t dev_t;
typedef unsigned long __ino_t;
typedef __ino_t ino_t;
typedef unsigned short __mode_t;
typedef __mode_t mode_t;
typedef unsigned int __nlink_t;
typedef __nlink_t nlink_t;
typedef unsigned int __uid_t;
typedef __uid_t uid_t;
typedef unsigned int __gid_t;
typedef __gid_t gid_t;
typedef unsigned long off_t;
typedef off_t off64_t;
typedef off64_t __off64_t;
typedef size_t __blksize_t;
typedef __blksize_t blksize_t;
typedef unsigned long __blkcnt_t;
typedef __blkcnt_t blkcnt_t;
typedef unsigned long long __blkcnt64_t;
typedef __blkcnt64_t blkcnt64_t;
typedef unsigned long __syscall_ulong_t;
typedef __syscall_ulong_t syscall_ulong_t;

int fork(void);






struct tm {
    int tm_sec;
    int tm_min;
    int tm_hour;
    int tm_mday;
    int tm_mon;
    int tm_year;
    int tm_wday;
    int tm_yday;
    int tm_isdst;
};

typedef long __time_t;
typedef __time_t time_t;
char *ctime_r(const time_t *timer, char *buf);
time_t time(time_t *t);
struct tm *localtime(const time_t *timer);
size_t strftime(char *s, size_t maxsize, const char *format,
                const struct tm *timeptr);
struct tm *localtime_r(const time_t *timep, struct tm *result);









struct stat {
    dev_t     st_dev;     /* ID of device containing file */
    ino_t     st_ino;     /* inode number */
    mode_t    st_mode;    /* protection */
    nlink_t   st_nlink;   /* number of hard links */
    uid_t     st_uid;     /* user ID of owner */
    gid_t     st_gid;     /* group ID of owner */
    dev_t     st_rdev;    /* ID of device (if special file) */
    off_t     st_size;    /* total size, in bytes */
    time_t    st_atime;   /* time of last access */
    time_t    st_mtime;   /* time of last modification */
    time_t    st_ctime;   /* time of last status change */
    blksize_t st_blksize; /* blocksize for filesystem I/O */
    blkcnt_t  st_blocks;  /* number of 512B blocks allocated */
};

//struct stat {
//  __dev_t st_dev;		/* Device.  */
//  unsigned short int __pad1;
//  __ino_t __st_ino;			/* 32bit file serial number.	*/
//  __mode_t st_mode;			/* File mode.  */
//  __nlink_t st_nlink;			/* Link count.  */
//  __uid_t st_uid;		/* User ID of the file's owner.	*/
//  __gid_t st_gid;		/* Group ID of the file's group.*/
//  __dev_t st_rdev;		/* Device number, if device.  */
//  unsigned short int __pad2;
//  __off64_t st_size;			/* Size of file, in bytes.  */
//  __blksize_t st_blksize;	/* Optimal block size for I/O.  */
//  __blkcnt64_t st_blocks;		/* Number 512-byte blocks allocated. */
//  __time_t st_atime;			/* Time of last access.  */
//  __syscall_ulong_t st_atimensec;	/* Nscecs of last access.  */
//  __time_t st_mtime;			/* Time of last modification.  */
//  __syscall_ulong_t st_mtimensec;	/* Nsecs of last modification.  */
//  __time_t st_ctime;			/* Time of last status change.  */
//  __syscall_ulong_t st_ctimensec;	/* Nsecs of last status change.  */
//  unsigned long int __unused4;
//  unsigned long int __unused5;
//};

int stat(const char * path, struct stat * buf);
int fstat(int fd, struct stat *buf);
int lstat(const char *path, struct stat *buf);
int fstatat(int filedes, const char *path, struct stat *buf, int flag);






int wait(int *status);
pid_t waitpid(pid_t pid, int *status, int options);









char *optarg;
int optind = 1;

char *getpass(const char *prompt);
int execvp(const char *file, char *const argv[]);
int isatty(int filedes);
int getopt(int argc, char *const argv[], const char *optstring);
pid_t getpid(void);
int execlp(const char *file, const char *arg0, ...);






int open(const char * pathname, int flags);
unsigned int read(int fd, void * buf, unsigned long count);
int close(int fd);






typedef struct {
    size_t gl_pathc;
    char **gl_pathv;
    size_t gl_off;
} glob_t;
int glob(const char *pattern, int flags,
         int (*errfunc)(const char *epath, int eerrno), glob_t *pglob);
void globfree(glob_t *pglob);









typedef struct Type Type;
typedef struct Node Node;
typedef struct Member Member;
typedef struct Relocation Relocation;
typedef struct Hideset Hideset;

//
// strings.c
//

typedef struct {
  char **data;
  int capacity;
  int len;
} StringArray;

void strarray_push(StringArray *arr, char *s);
char *format(char *fmt, ...)  ;

//
// tokenize.c
//

// Token
typedef enum {
  TK_IDENT,   // Identifiers
  TK_PUNCT,   // Punctuators
  TK_KEYWORD, // Keywords
  TK_STR,     // String literals
  TK_NUM,     // Numeric literals
  TK_PP_NUM,  // Preprocessing numbers
  TK_EOF,     // End-of-file markers
} TokenKind;

typedef struct {
  char *name;
  int file_no;
  char *contents;

  // For #line directive
  char *display_name;
  int line_delta;
} File;

// Token type
typedef struct Token Token;
struct Token {
  TokenKind kind;   // Token kind
  Token *next;      // Next token
  int64_t val;      // If kind is TK_NUM, its value
  long double fval; // If kind is TK_NUM, its value
  char *loc;        // Token location
  int len;          // Token length
  Type *ty;         // Used if TK_NUM or TK_STR
  char *str;        // String literal contents including terminating '\0'

  File *file;       // Source location
  char *filename;   // Filename
  int line_no;      // Line number
  int line_delta;   // Line number
  bool at_bol;      // True if this token is at beginning of line
  bool has_space;   // True if this token follows a space character
  Hideset *hideset; // For macro expansion
  Token *origin;    // If this is expanded from a macro, the original token
};

void error(char *fmt, ...)  ;
void error_at(char *loc, char *fmt, ...)  ;
void error_tok(Token *tok, char *fmt, ...)  ;
void warn_tok(Token *tok, char *fmt, ...)  ;
bool equal(Token *tok, char *op);
Token *skip(Token *tok, char *op);
bool consume(Token **rest, Token *tok, char *str);
void convert_pp_tokens(Token *tok);
File **get_input_files(void);
File *new_file(char *name, int file_no, char *contents);
Token *tokenize_string_literal(Token *tok, Type *basety);
Token *tokenize(File *file);
Token *tokenize_file(char *filename);



//
// preprocess.c
//

char *search_include_paths(char *filename);
void init_macros(void);
void define_macro(char *name, char *buf);
void undef_macro(char *name);
Token *preprocess(Token *tok);

//
// parse.c
//

// Variable or function
typedef struct Obj Obj;
struct Obj {
  Obj *next;
  char *name;    // Variable name
  Type *ty;      // Type
  Token *tok;    // representative token
  bool is_local; // local or global/function
  int align;     // alignment

  // Local variable
  int offset;

  // Global variable or function
  bool is_function;
  bool is_definition;
  bool is_static;

  // Global variable
  bool is_tentative;
  bool is_tls;
  char *init_data;
  Relocation *rel;

  // Function
  bool is_inline;
  Obj *params;
  Node *body;
  Obj *locals;
  Obj *va_area;
  Obj *alloca_bottom;
  int stack_size;

  // Static inline function
  bool is_live;
  bool is_root;
  StringArray refs;
};

// Global variable can be initialized either by a constant expression
// or a pointer to another global variable. This struct represents the
// latter.
struct Relocation {
  Relocation *next;
  int offset;
  char **label;
  long addend;
};

// AST node
typedef enum {
  ND_NULL_EXPR, // Do nothing
  ND_ADD,       // +
  ND_SUB,       // -
  ND_MUL,       // *
  ND_DIV,       // /
  ND_NEG,       // unary -
  ND_MOD,       // %
  ND_BITAND,    // &
  ND_BITOR,     // |
  ND_BITXOR,    // ^
  ND_SHL,       // <<
  ND_SHR,       // >>
  ND_EQ,        // ==
  ND_NE,        // !=
  ND_LT,        // <
  ND_LE,        // <=
  ND_ASSIGN,    // =
  ND_COND,      // ?:
  ND_COMMA,     // ,
  ND_MEMBER,    // . (struct member access)
  ND_ADDR,      // unary &
  ND_DEREF,     // unary *
  ND_NOT,       // !
  ND_BITNOT,    // ~
  ND_LOGAND,    // &&
  ND_LOGOR,     // ||
  ND_RETURN,    // "return"
  ND_IF,        // "if"
  ND_FOR,       // "for" or "while"
  ND_DO,        // "do"
  ND_SWITCH,    // "switch"
  ND_CASE,      // "case"
  ND_BLOCK,     // { ... }
  ND_GOTO,      // "goto"
  ND_GOTO_EXPR, // "goto" labels-as-values
  ND_LABEL,     // Labeled statement
  ND_LABEL_VAL, // [GNU] Labels-as-values
  ND_FUNCALL,   // Function call
  ND_EXPR_STMT, // Expression statement
  ND_STMT_EXPR, // Statement expression
  ND_VAR,       // Variable
  ND_VLA_PTR,   // VLA designator
  ND_NUM,       // Integer
  ND_CAST,      // Type cast
  ND_MEMZERO,   // Zero-clear a stack variable
  ND_ASM,       // "asm"
  ND_CAS,       // Atomic compare-and-swap
  ND_EXCH,      // Atomic exchange
} NodeKind;

// AST node type
struct Node {
  NodeKind kind; // Node kind
  Node *next;    // Next node
  Type *ty;      // Type, e.g. int or pointer to int
  Token *tok;    // Representative token

  Node *lhs;     // Left-hand side
  Node *rhs;     // Right-hand side

  // "if" or "for" statement
  Node *cond;
  Node *then;
  Node *els;
  Node *init;
  Node *inc;

  // "break" and "continue" labels
  char *brk_label;
  char *cont_label;

  // Block or statement expression
  Node *body;

  // Struct member access
  Member *member;

  // Function call
  Type *func_ty;
  Node *args;
  bool pass_by_stack;
  Obj *ret_buffer;

  // Goto or labeled statement, or labels-as-values
  char *label;
  char *unique_label;
  Node *goto_next;

  // Switch
  Node *case_next;
  Node *default_case;

  // Case
  long begin;
  long end;

  // "asm" string literal
  char *asm_str;

  // Atomic compare-and-swap
  Node *cas_addr;
  Node *cas_old;
  Node *cas_new;

  // Atomic op= operators
  Obj *atomic_addr;
  Node *atomic_expr;

  // Variable
  Obj *var;

  // Numeric literal
  int64_t val;
  long double fval;
};

Node *new_cast(Node *expr, Type *ty);
int64_t const_expr(Token **rest, Token *tok);
Obj *parse(Token *tok);

//
// type.c
//

typedef enum {
  TY_VOID,
  TY_BOOL,
  TY_CHAR,
  TY_SHORT,
  TY_INT,
  TY_LONG,
  TY_FLOAT,
  TY_DOUBLE,
  TY_LDOUBLE,
  TY_ENUM,
  TY_PTR,
  TY_FUNC,
  TY_ARRAY,
  TY_VLA, // variable-length array
  TY_STRUCT,
  TY_UNION,
} TypeKind;

struct Type {
  TypeKind kind;
  int size;           // sizeof() value
  int align;          // alignment
  bool is_unsigned;   // unsigned or signed
  bool is_atomic;     // true if _Atomic
  Type *origin;       // for type compatibility check

  // Pointer-to or array-of type. We intentionally use the same member
  // to represent pointer/array duality in C.
  //
  // In many contexts in which a pointer is expected, we examine this
  // member instead of "kind" member to determine whether a type is a
  // pointer or not. That means in many contexts "array of T" is
  // naturally handled as if it were "pointer to T", as required by
  // the C spec.
  Type *base;

  // Declaration
  Token *name;
  Token *name_pos;

  // Array
  int array_len;

  // Variable-length array
  Node *vla_len; // # of elements
  Obj *vla_size; // sizeof() value

  // Struct
  Member *members;
  bool is_flexible;
  bool is_packed;

  // Function type
  Type *return_ty;
  Type *params;
  bool is_variadic;
  Type *next;
};

// Struct member
struct Member {
  Member *next;
  Type *ty;
  Token *tok; // for error message
  Token *name;
  int idx;
  int align;
  int offset;

  // Bitfield
  bool is_bitfield;
  int bit_offset;
  int bit_width;
};

extern Type *ty_void;
extern Type *ty_bool;

extern Type *ty_char;
extern Type *ty_short;
extern Type *ty_int;
extern Type *ty_long;

extern Type *ty_uchar;
extern Type *ty_ushort;
extern Type *ty_uint;
extern Type *ty_ulong;

extern Type *ty_float;
extern Type *ty_double;
extern Type *ty_ldouble;

bool is_integer(Type *ty);
bool is_flonum(Type *ty);
bool is_numeric(Type *ty);
bool is_compatible(Type *t1, Type *t2);
Type *copy_type(Type *ty);
Type *pointer_to(Type *base);
Type *func_type(Type *return_ty);
Type *array_of(Type *base, int size);
Type *vla_of(Type *base, Node *expr);
Type *enum_type(void);
Type *struct_type(void);
void add_type(Node *node);

//
// codegen.c
//

void codegen(Obj *prog, FILE *out);
int align_to(int n, int align);

//
// unicode.c
//

int encode_utf8(char *buf, uint32_t c);
uint32_t decode_utf8(char **new_pos, char *p);
bool is_ident1(uint32_t c);
bool is_ident2(uint32_t c);
int display_width(char *p, int len);

//
// hashmap.c
//

typedef struct {
  char *key;
  int keylen;
  void *val;
} HashEntry;

typedef struct {
  HashEntry *buckets;
  int capacity;
  int used;
} HashMap;

void *hashmap_get(HashMap *map, char *key);
void *hashmap_get2(HashMap *map, char *key, int keylen);
void hashmap_put(HashMap *map, char *key, void *val);
void hashmap_put2(HashMap *map, char *key, int keylen, void *val);
void hashmap_delete(HashMap *map, char *key);
void hashmap_delete2(HashMap *map, char *key, int keylen);
void hashmap_test(void);

//
// main.c
//

bool file_exists(char *path);

extern StringArray include_paths;
extern bool opt_fpic;
extern bool opt_fcommon;
extern char *base_file;


// Scope for local variables, global variables, typedefs
// or enum constants
typedef struct {
  Obj *var;
  Type *type_def;
  Type *enum_ty;
  int enum_val;
} VarScope;

// Represents a block scope.
typedef struct Scope Scope;
struct Scope {
  Scope *next;

  // C has two block scopes; one is for variables/typedefs and
  // the other is for struct/union/enum tags.
  HashMap vars;
  HashMap tags;
};

// Variable attributes such as typedef or extern.
typedef struct {
  bool is_typedef;
  bool is_static;
  bool is_extern;
  bool is_inline;
  bool is_tls;
  int align;
} VarAttr;

// This struct represents a variable initializer. Since initializers
// can be nested (e.g. `int x[2][2] = {{1, 2}, {3, 4}}`), this struct
// is a tree data structure.
typedef struct Initializer Initializer;
struct Initializer {
  Initializer *next;
  Type *ty;
  Token *tok;
  bool is_flexible;

  // If it's not an aggregate type and has an initializer,
  // `expr` has an initialization expression.
  Node *expr;

  // If it's an initializer for an aggregate type (e.g. array or struct),
  // `children` has initializers for its children.
  Initializer **children;

  // Only one member can be initialized for a union.
  // `mem` is used to clarify which member is initialized.
  Member *mem;
};

// For local variable initializer.
typedef struct InitDesg InitDesg;
struct InitDesg {
  InitDesg *next;
  int idx;
  Member *member;
  Obj *var;
};

// All local variable instances created during parsing are
// accumulated to this list.
static Obj *locals;

// Likewise, global variables are accumulated to this list.
static Obj *globals;

//static Scope *scope = &(Scope){};
static Scope scopetmp = {};
static Scope * scope = &scope;

// Points to the function object the parser is currently parsing.
static Obj *current_fn;

// Lists of all goto statements and labels in the curent function.
static Node *gotos;
static Node *labels;

// Current "goto" and "continue" jump targets.
static char *brk_label;
static char *cont_label;

// Points to a node representing a switch if we are parsing
// a switch statement. Otherwise, NULL.
static Node *current_switch;

static Obj *builtin_alloca;
void * calloc(int);
bool equal(Token *tok, char *op);
bool consume(Token **rest, Token *tok, char *str);

static void struct_members(Type *ty) {
  Member head = {};
  Member *cur = &head;
  while (!equal(0, 1)) {
    VarAttr attr = {};
    Type *basety = consume(0, 0, &attr);
    while (consume(0, 0, 1)) {
      Member *mem = calloc(1, 2);
      if (consume(0, 0, 0)) {
        mem->bit_width = 1;
      }
      cur = cur->next;
    }
  }
}
