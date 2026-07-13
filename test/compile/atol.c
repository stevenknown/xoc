long xatol(char * nptr)
{
    if (nptr == 0) return 0;
    while (*nptr == ' ' || *nptr == '\t') {
        nptr++;
    }
    char sign = *nptr;
    if (sign == '-' || sign == '+') {
        nptr++;
    }
    long res = 0;
    if (nptr[0] == '0' && (nptr[1] == 'x' || nptr[1] == 'X')) { //hex
        nptr += 2;
        char c = *nptr;
        while ((c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')) {
            if (c >= '0' && c <= '9') {
                res = 16 * res + c - '0';
            } else if (c >= 'A' && c <= 'F') {
                res = 16 * res + c - 'A' + 10;
            } else if (c >= 'a' && c <= 'f') {
                res = 16 * res + c - 'a' + 10;
            } else {
                return 0;
            }
            c = *++nptr;
        }
    } else { //decimal
        while (*nptr >= '0' && *nptr <= '9') {
            res = 10 * res + (*nptr - '0'); //accumulate digit
            nptr++;
        }
    }

    if (sign == '-') {
        return -res;
    }
    return res;
}
