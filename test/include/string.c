int tolower(int c)
{
    if (c >= 'A' && c <= 'Z') {
        return c + ('a' - 'A');
    }
    return c;
}
int strncasecmp(const char *s1, const char *s2, size_t n)
{
    while (n-- > 0 && *s1 && *s2) {
        if (tolower((unsigned char)*s1) != tolower((unsigned char)*s2)) {
            return -1;
        }
        s1++;
        s2++;
    }
    return 0;
}
