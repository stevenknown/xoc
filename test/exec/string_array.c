int printf(char const*,...);
int main()
{
    char temp;
    char frontpos;
    char backpos;
    int i;
    for (i = 0; i < 120; i++) {
        temp="                                                                " " TVGH  CD  M KN   YSAABW R       TVGH  CD  M KN   YSAABW R"[i];
        frontpos="                                                                " " TVGH  CD  M KN   YSAABW R       TVGH  CD  M KN   YSAABW R"[i];
        backpos=temp;
        printf("\n%c,%c,%c", temp, frontpos, backpos);
    }
    return 0;
}

