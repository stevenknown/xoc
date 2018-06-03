CC := $(shell which clang++ > /dev/null)
ifndef CC
CC = $(if $(shell which clang), clang, gcc)
endif

OBJS += main.o

CFLAGS = -DFOR_DEX -D_DEBUG_ -O0 -g2

grreader: objs
    $(CC) $(OBJS) $(CFLAGS) -L../.. -lxoc -o grreader.exe -lstdc++ -lm
    @echo "success!!"

INC=-I .
%.o:%.cpp
    @echo "build $<"
    $(CC) $(CFLAGS) $(INC) -c $< -o $@

objs: $(OBJS)
