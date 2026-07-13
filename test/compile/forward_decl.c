struct _IO_FILE;
typedef struct _IO_FILE FILE; //typedef allows referring an incomplete type.
typedef struct _IO_FILE __FILE;
struct _IO_marker {
  int _pos;
  struct _IO_FILE *_sbuf;
};
struct _IO_FILE {
  int _flags;
  struct _IO_FILE *_chain;
};
typedef struct _IO_FILE _IO_FILE;
