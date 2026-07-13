#ifndef _STDARG_H
#define _STDARG_H

/* The macro __STDC__ is defined by all versions of the ANSI C compiler.

   The macro __STDC_VERSION__ is defined by ANSI C compilers that
   support the C99 standard.  It is not defined by pre-C99 versions.

   The macro __STDC_HOSTED__ is defined by all ANSI C implementations
   that are not hosted implementations (i.e., they are freestanding).

   The macro __STDC_NO_VLAs__ is defined by implementations that do not
   support C99 variable-length arrays, whether or not they are otherwise
   C99 compliant.  */

#if defined (_STDC_NO_VLAs) || (!defined (_STDC_VERSION__) && !defined (_STDC_HOSTED__))
#define _STDARG_H
#endif

/* All the headers included by stdarg.h itself.  */
//#include <stdarg.h>

/* Define __va_list_tag to indicate that the file is being used properly.  */
#define __va_list_tag

/* The macro for declaring a variable of type `va_list'.  */
#define va_list unsigned char*

/* The macro for starting the use of the variable `ap'.  */
#define va_start(ap, last) ap = (((unsigned char*)&last) + sizeof(last))

/* The macro for ending the use of the variable `ap'.  */
#define va_end(ap) ap

/* The macro for getting the next argument of type `type'.  */
#define va_arg(ap, type) (ap += sizeof(type))

/* The macro for copying the `next argument' state from `from' to `to'.  */
#define va_copy(dest, src) (dest = src)

#endif


