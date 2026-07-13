extern int errno;
typedef long unsigned int size_t;
char *dirname(const char *path);
char *basename(const char *path);
typedef int bool;
typedef signed char        int8_t;
typedef unsigned char       uint8_t;

typedef signed short       int16_t;
typedef unsigned short      uint16_t;

typedef signed int          int32_t;
typedef unsigned int        uint32_t;

typedef signed long long   int64_t;
typedef unsigned long long uint64_t;

typedef signed int intptr_t;
int atoi(const char *str);
void * calloc(int);
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

char *optarg;
int optind = 1;

typedef struct {
    size_t gl_pathc;
    char **gl_pathv;
    size_t gl_off;
} glob_t;
typedef struct Type Type;
typedef struct Node Node;
typedef struct Member Member;
typedef struct Relocation Relocation;
typedef struct Hideset Hideset;
typedef struct {
  char **data;
  int capacity;
  int len;
} StringArray;

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

void error_tok(Token *tok, char *fmt, ...)  ;
Token *tokenize_string_literal();
//START
static Token *copy_token(Token *tok);
char *search_include_paths(char *filename);
void init_macros(void);
void define_macro(char *name, char *buf);
void undef_macro(char *name);
Token *preprocess(Token *tok);
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
struct Relocation {
  Relocation *next;
  int offset;
  char **label;
  long addend;
};
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
  Type *base;
  Token *name;
  Token *name_pos;
  int array_len;
  Node *vla_len; // # of elements
  Obj *vla_size; // sizeof() value
  Member *members;
  bool is_flexible;
  bool is_packed;
  Type *return_ty;
  Type *params;
  bool is_variadic;
  Type *next;
};
struct Member {
  Member *next;
  Type *ty;
  Token *tok; // for error message
  Token *name;
  int idx;
  int align;
  int offset;
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
bool file_exists(char *path);
extern StringArray include_paths;
extern bool opt_fpic;
extern bool opt_fcommon;
extern char *base_file;
typedef struct MacroParam MacroParam;
struct MacroParam {
  MacroParam *next;
  char *name;
};

typedef struct MacroArg MacroArg;
struct MacroArg {
  MacroArg *next;
  char *name;
  bool is_va_args;
  Token *tok;
};

typedef Token *macro_handler_fn(Token *);

typedef struct Macro Macro;
struct Macro {
  char *name;
  bool is_objlike; // Object-like or function-like
  MacroParam *params;
  char *va_args_name;
  Token *body;
  macro_handler_fn *handler;
};

typedef enum { IN_THEN, IN_ELIF, IN_ELSE } CFS;

// `#if` can be nested, so we use a stack to manage nested `#if`s.
typedef struct CondIncl CondIncl;
struct CondIncl {
  CondIncl *next;
  CFS ctx;
  Token *tok;
  bool included;
};

typedef struct Hideset Hideset;
struct Hideset {
  Hideset *next;
  char *name;
};

typedef enum {
  TSTR,
  TEOF,
} TokenKind;


typedef struct tagS { int * next; } S;
void tko();
void join(S *tok) {
  for (S *tok1 = tok; tok1 != TEOF;) {
    if (tok1 > 1)
      for (int *t = 0; t == TSTR;)
          tko();
    while (tok1 == TSTR)
      tok1 = tok1->next;
  }
}
