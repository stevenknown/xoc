#include "stdio.h"
#include "../xcominc.h"
class CompareString {
public:
    bool is_equ(char const* s1, char const* s2) const
    {
        return xcom::xstrcmp(s1, s2) == 0;
    }

    bool is_less(char const* s1, char const* s2) const
    {
        while (s1 != nullptr && s2 != nullptr) {
            if (*s1 < *s2) { return true; }
            s1++;
            s2++;
        }
        if (s1 == nullptr) { return true; }
        return false;
    }

    char const* createKey(char const* t) { return t; }
};
void string_tab()
{
    TTab<char const*, CompareString> tab;
    tab.append("Mike");
    tab.append("Tom");
    tab.append("Jerry");

    printf("\nIterate tab:\n");
    TTabIter<char const*> iter;

    for (char const* v = tab.get_first(iter);
         v != nullptr; v = tab.get_next(iter)) {
        printf("%s\n", v);
    }

    printf("%d\n", tab.get_elem_count());
}


void int_tab()
{
    TTab<int> tab;
    tab.append(123);
    tab.append(234);
    tab.append(345);

    printf("\nIterate tab:\n");
    TTabIter<int> iter;

    //If you are going to use integer to judge if the iteration
    //determinate, you should guarantee 0 is not in the TTab.
    for (int v = tab.get_first(iter);
         v != 0; v = tab.get_next(iter)) {
        printf("%d\n", v);
    }

    printf("\nIterate tab via iter itself:\n");
    for (int v = tab.get_first(iter);
         iter.get_elem_count() != 0; v = tab.get_next(iter)) {
        printf("%d\n", v);
    }

    printf("%d\n", tab.get_elem_count());
}


int main()
{
    int_tab();
    string_tab();
    return 0;
}
