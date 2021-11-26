CC := $(shell which clang++ > /dev/null)
ifndef CC
  CC = $(if $(shell which clang), clang, gcc)
endif

include reader/Makefile.inc
include com/Makefile.inc
include opt/Makefile.inc

ifdef WIN
#Add predefined macro if build host is Windows.
CFLAGS += -D_ON_WINDOWS_
endif

CFLAGS += -DFOR_DEX -D_DEBUG_ -D_SUPPORT_C11_ -O2 -g2 -Wno-unknown-pragmas \
          -Wno-write-strings -Wsign-promo -Wsign-compare -Wpointer-arith \
          -Wno-multichar -Winit-self -Wswitch

SRC = .

ifneq (,$(filter $(CC),g++ gcc))
	CFLAGS += -Wstrict-aliasing=3 -finline-limit=10000000
endif

all: com_objs opt_objs reader_objs
	ar rcs libxoc.a $(COM_OBJS) $(OPT_OBJS) $(READER_OBJS)
	@echo "success!!"

INC=-I .
%.o:%.cpp
	@echo "build $<"
	$(CC) $(CFLAGS) $(INC) -c $< -o $@

com_objs: $(COM_OBJS)
opt_objs: $(OPT_OBJS)
reader_objs: $(READER_OBJS)

clean:
	@find ./ -name "*.gcda" | xargs rm -f
	@find ./ -name "*.gcno" | xargs rm -f
	@find ./ -name "*.o" | xargs rm -f
	@find ./ -name "*.a" | xargs rm -f
	@find ./ -name "*.dot" | xargs rm -f
	@find ./ -name "*.exe" | xargs rm -f
	@find ./ -name "*.elf" | xargs rm -f
	@find ./ -name "*.out" | xargs rm -f
	@find ./ -name "*.tmp" | xargs rm -f
	@find ./ -name "*.vcg" | xargs rm -f
	@find ./ -name "*.cxx" | xargs rm -f
	@find ./ -name "*.asm" | xargs rm -f
	@find ./ -name "*.swp" | xargs rm -f
	@find ./ -name "*.swo" | xargs rm -f
	@find ./ -name "*.log" | xargs rm -f
	@find ./ -name "*.LOGLOG" | xargs rm -f
	@find ./ -name "LOGLOG" | xargs rm -f

