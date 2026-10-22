#!/bin/bash
make -j1 -f Makefile.xinfer TARG=FOR_ARM TARG_DIR="arm" DEBUG=true FOR_IP=true REF_TARGMACH_INFO=false THREAD_NUM=16
