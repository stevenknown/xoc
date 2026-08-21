void printf(char const*,...);
int main(void)
{
    char pal[] = "&.:-=+*#%@";
    int j=0;
    printf("\nsizeof=%u\n",sizeof(pal));
    printf("pal[%u]=%c\n", j, pal[j]); j++; //' '
    printf("pal[%u]=%c\n", j, pal[j]); j++; //'.'
    printf("pal[%u]=%c\n", j, pal[j]); j++; //':'
    printf("pal[%u]=%c\n", j, pal[j]); j++; //'-'
    printf("pal[%u]=%c\n", j, pal[j]); j++; //'='
    printf("pal[%u]=%c\n", j, pal[j]); j++; //'+'
    printf("pal[%u]=%c\n", j, pal[j]); j++; //'*'
    printf("pal[%u]=%c\n", j, pal[j]); j++; //'#'
    printf("pal[%u]=%c\n", j, pal[j]); j++; //'%'
    printf("pal[%u]=%c\n", j, pal[j]); j++; //'@'
    printf("\n");
    if (sizeof(pal) != 11) { return 1; }
    return 0;
}
