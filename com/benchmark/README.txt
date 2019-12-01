Benchmark is a concise and simple program to evaluate the runtime performance
of library and application.

test_smempool.cpp:
    Evaluate the runtime performance of memory pool.
    command line:
      >g++ test_smempool.cpp ../smempool.cpp -DRUN_STL; time ./a.out
      >g++ test_smempool.cpp ../smempool.cpp; time ./a.out

test_list.cpp:
    Evaluate the runtime performance of List structure.
    command line:
      >g++ test_list.cpp -DRUN_STL; time ./a.out
      >g++ test_list.cpp; time ./a.out

test_map.cpp:
    Evaluate the runtime performance of map structure.
    command line:
      >g++ test_map.cpp -std=c++11 -DRUN_STL; time ./a.out
      >g++ test_map.cpp ../smempool.cpp; time ./a.out

