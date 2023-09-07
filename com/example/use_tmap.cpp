#include "stdio.h"
#include "../xcominc.h"
int main()
{
    TMap<int, char const*> map;
    map.set(123, "Mike");
    map.set(234, "Charlie");
    map.set(345, "Tom");

    printf("\nIterate map via string pointer:\n");
    TMapIter<int, char const*> iter;
    char const* str;
    for (int v = map.get_first(iter, &str);
         str != nullptr; v = map.get_next(iter, &str)) {
        printf("%d->%s\n", v, str);
    }

    printf("\nIterate map via iter itself:\n");
    for (int v = map.get_first(iter, &str);
         iter.get_elem_count() != 0; v = map.get_next(iter, &str)) {
        printf("%d->%s\n", v, str);
    }

    printf("%d\n", map.get_elem_count());

    return 0;
}
