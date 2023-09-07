OPTION="-O0 -g2"
FILE="../bs.cpp ../smempool.cpp ../fileobj.cpp
      ../byteop.cpp ../diagnostic.cpp ../sbs.cpp"
g++ $OPTION use_bs.cpp $FILE -I.. -o use_bs.exe
./use_bs.exe
g++ $OPTION use_list.cpp $FILE -I.. -o use_list.exe
./use_list.exe
g++ $OPTION use_slist.cpp $FILE -I.. -o use_slist.exe
./use_slist.exe
g++ $OPTION use_tmap.cpp $FILE -I.. -o use_tmap.exe
./use_tmap.exe
g++ $OPTION use_ttab.cpp $FILE -I.. -o use_ttab.exe
./use_ttab.exe
g++ $OPTION use_vector.cpp $FILE -I.. -o use_vector.exe
./use_vector.exe
g++ $OPTION testbs.cpp $FILE -I.. -D_DEBUG_ -o testbs.exe
./testbs.exe
