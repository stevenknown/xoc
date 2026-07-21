typedef struct {
    int line;
    int column;
} File;
File *f;
void unreadc(int c)
{
    if (c == 10) {
        f->column = 1;
        f->line = 2;
    } else {
        f->column--;
    }
}
