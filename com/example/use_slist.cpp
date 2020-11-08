#include "stdio.h"
#include "ltype.h"
#include "comf.h"
#include "smempool.h"
#include "sstl.h"
using namespace xcom;
void use_slist_core()
{
    //Single List Core need user declared a mempool.
    SMemPool * pool = smpoolCreate(sizeof(SC<int>), MEM_CONST_SIZE);

    //Define a free list
    SC<int> * free_list = nullptr;

    //Of course you can use "new" to allocate.
    SListCore<int> * l = (SListCore<int>*)malloc(sizeof(SListCore<int>));

    //You need to invoke the init() to do initialization.
    l->init();

    SC<int> * ct = l->append_head(4, &free_list, pool); //append a new element at the beginning.

    //Insert "5" after position of first element
    //place before second element.
    ct = l->insert_after(5, ct, &free_list, pool);

    //Insert "6" after "5".
    l->insert_after(6, ct, &free_list, pool);

    printf("Iter list element by user declared iter:\n");
    for (SC<int> * iter = l->get_head();
        iter != l->end(); iter = l->get_next(iter)) {
        printf("%d\n", iter->val());
    }

    //You can not access the last element unless using the SListEx.

    printf("%d\n", l->get_elem_count());

    l->clean(&free_list);
    free(l);
    smpoolDelete(pool);
}


void use_slist()
{
    //Single List need user declared a mempool.
    SMemPool * pool = smpoolCreate(sizeof(SC<int>), MEM_CONST_SIZE);
    SList<int> l(pool);

    //Or you can initialize pool via l.set_pool(pool) as well.

    SC<int> * ct = l.append_head(1); //append a new element at the beginning.

    //Insert "2" after position of first element
    //place before second element.
    ct = l.insert_after(2, ct);

    //Insert "3" after "2".
    l.insert_after(3, ct);

    printf("Iter list element by user declared iter:\n");
    for (SC<int> * iter = l.get_head();
        iter != l.end(); iter = l.get_next(iter)) {
        printf("%d\n", iter->val());
    }

    //You can not access the last element unless using the SListEx.

    printf("%d\n", l.get_elem_count());

    smpoolDelete(pool);
}


int main()
{
    use_slist();
    use_slist_core();
    return 0;
}
