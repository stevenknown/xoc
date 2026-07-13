typedef enum Kind {
    NORMAL = 0,
    GENERATOR = (1 << 0),
    ASYNC = (1 << 1),
    ASYNC_GENERATOR = (GENERATOR | ASYNC),
} Kind;

int main()
{
    return 0;
}
