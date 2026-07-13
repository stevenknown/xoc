#ifndef _STDBOOL_H_
#define _STDBOOL_H_

/* All the definitions below are implementation-specific.  */

/* Thestdbool.h header shall define _Bool and the constants true and
   false.  If the implementation provides the stdbool.h header, it
   shall define these macros in the std namespace.  */

typedef int bool;
#define true 1
#define false 0
#define _Bool bool

#endif
