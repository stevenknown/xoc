Benchmark is a concise and simple program to evaluate the runtime performance
of library and application.

test_smempool.cpp:
    Evaluate the runtime performance of memory pool.
    command line:
      >g++ -std=c++0x test_smempool.cpp ../smempool.cpp -lstdc++ -DRUN_STL; time ./a.out
      >g++ -std=c++0x test_smempool.cpp ../smempool.cpp -lstdc++; time ./a.out

test_list.cpp:
    Evaluate the runtime performance of List structure.
    command line:
      >g++ -std=c++0x test_list.cpp -DRUN_STL -lstdc++; time ./a.out
      >g++ -std=c++0x test_list.cpp -lstdc++; time ./a.out

test_map.cpp:
    Evaluate the runtime performance of map structure.
    command line:
      >g++ -std=c++0x test_map.cpp -DRUN_STL -lstdc++; time ./a.out
      >g++ -std=c++0x test_map.cpp ../smempool.cpp -lstdc++; time ./a.out

