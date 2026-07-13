char *dirname(const char *path)
{
    if (path == NULL || *path == '\0') {
        return NULL;
    }
    char *last_slash = strrchr(path, '/');
    if (last_slash == path || *(last_slash - 1) == '/') {
        return "/";
    } else if (last_slash != NULL) {
        *last_slash = '\0';
    } else {
        return ".";
    }
    return (char *)path;
}


char *basename(const char *path)
{
    const char *last_slash = strrchr(path, '/');
    if (last_slash == NULL || *(last_slash - 1) == '/') {
        return (char *)path;
    }
    return (char *)(last_slash + 1);
}
