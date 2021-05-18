Build all examples:
    ./build.sh

If you are building testbs.cpp, the macro -D_LINUX_ is needed.
    e.g: g++ -std=c++0x testbs.cpp ../smempool.cpp ../bs.cpp -I.. -D_LINUX_ -lstdc++
