pwd
make clean -f Makefile.xoc DEBUG=true TARG=FOR_DEX TARG_DIR=dex
make -f Makefile.xoc DEBUG=true TARG=FOR_DEX TARG_DIR=dex THREAD_NUM=32 
