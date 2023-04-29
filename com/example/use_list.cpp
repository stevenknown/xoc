#include "stdio.h"
#include "../xcominc.h"
int main()
{
    List<int> l;

    l.append_tail(0); //append a new element at the end.
    C<int> * ct = l.append_head(1); //append a new element at the beginning.

    //Insert "2" after position of first element
    //place before second element.
    l.insert_after(2, ct);

    //Insert "3" before the last element.
    //Note this function will more slower than the above.
    l.insert_before(3, 0);

    printf("Iter list element by internal iter:\n");
    int i = 0;
    for (int e = l.get_head(); i < l.get_elem_count(); i++, e = l.get_next()) {
        printf("%d\n", e);
    }

    printf("Iter list element by user declared iter:\n");
    C<int> * iter;
    for (l.get_head(&iter); iter != nullptr; l.get_next(&iter)) {
        printf("%d\n", iter->val());
    }

    printf("\nReverse Iterator:\n");
    for (l.get_tail(&iter); iter != nullptr; l.get_prev(&iter)) {
        printf("%d\n", iter->val());
    }

    printf("\n\n\n");
    printf("%d\n", l.get_elem_count());
    printf("%d\n", l.get_head_nth(2));

    return 0;
}
