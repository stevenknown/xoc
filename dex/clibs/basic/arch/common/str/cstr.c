/*Copyright 2011 Alibaba Group*/
#include "str/cstr.h"
#include <string.h>

void cStrcrep(char* str, char pat, char rep) {
    if(str == NULL)
        return;
    while(*str) {
        if(*str == pat)
            *str = rep;
        ++str;
    }
}

Int32 cStrLastIndexOf(const char* str, char c) {
    Int32 i;
    Int32 last = -1;

    i = 0;
    while(str[i]) {
        if(str[i] == c)
            last = i;
        ++i;
    }
    return last;
}

#ifdef WIN32
#define HAVE_NO_STRTOK_R
#endif

#ifdef HAVE_NO_STRTOK_R

/* Reentrant string tokenizer.  Generic version.
Copyright (C) 1991,1996-1999,2001,2004 Free Software Foundation, Inc.
This file is part of the GNU C Library.

The GNU C Library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

The GNU C Library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with the GNU C Library; if not, write to the Free
Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
02111-1307 USA.  */

# define __strtok_r strtok_r
# define __rawmemchr strchr

char *
__strtok_r (char *s, const char *delim, char **save_ptr)
{
    char *token;

    if (s == NULL)
        s = *save_ptr;

    /* Scan leading delimiters.  */
    s += strspn (s, delim);
    if (*s == '\0')
    {
        *save_ptr = s;
        return NULL;
    }

    /* Find the end of the token.  */
    token = s;
    s = strpbrk (token, delim);
    if (s == NULL)
        /* This token finishes the string.  */
        *save_ptr = __rawmemchr (token, '\0');
    else
    {
        /* Terminate the token and make *SAVE_PTR point past it.  */
        *s = '\0';
        *save_ptr = s + 1;
    }
    return token;
}
#endif /* HAVE_NO_STRTOK_R */
