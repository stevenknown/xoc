int reduce()
{
    /*
    reduce
        for (i=0;i<5;i++) {
            if(i) continue;
            if(i) break;
        }
        into
            5
    */
    int i;
    for (i=0;i<5;i++) {
        if(i) continue;
        if(i) break;
    }
    return i;
}
