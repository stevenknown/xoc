Build the example:
    g++ use_vector.cpp -I.. -o a.out
or
    ./build.sh use_vector.cpp

If the example invokes functions in smempool.cpp, you can run example files by build_with_pool.sh.

If you are building testbs.cpp, the macro -D_LINUX_ is needed.
    e.g: g++ your_main.cpp testbs.cpp ../smempool.cpp ../bs.cpp -I.. -D_LINUX_
