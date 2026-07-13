typedef long unsigned int size_t;

extern size_t strlen (const char *__s);
extern void *malloc (size_t __size);
extern void free (void *__ptr);

typedef struct _krb5_data {
  unsigned int length;
  char *data;
} krb5_data;

typedef struct _krb5_error {
  krb5_data text;
} krb5_error;

extern const char *error_message (int);

int
recvauth_common (int problem)
{
  if (problem) {
    krb5_error error;
    const char *message = error_message(problem);
    error.text.length = strlen(message) + 1;
    if (!(error.text.data = malloc(error.text.length))) {
      goto cleanup;
    }
    free(error.text.data);
  }

 cleanup:
  return problem; /* { dg-bogus "leak" } */
}
