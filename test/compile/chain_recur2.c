char foo()
{
    char str[] = "ssssssssss";
    for (char * p = str; *p != 0; p++) { *p = 20; }
    return str[9];
}
