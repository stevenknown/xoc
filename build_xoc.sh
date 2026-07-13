THREAD_NUM=16

#Set target to FOR_DEX.
make clean -f Makefile.xoc DEBUG=true TARG=FOR_DEX TARG_DIR=dex

#Build the libxoc.a
make -j$THREAD_NUM -f Makefile.xoc DEBUG=true TARG=FOR_DEX TARG_DIR=dex THREAD_NUM=$THREAD_NUM
