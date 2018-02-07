CC := $(shell which clang++ > /dev/null)
ifndef CC
CC = $(if $(shell which clang), clang, gcc)
endif

EX1_OBJS += ex1.o

CFLAGS = -DFOR_DEX -D_DEBUG_ -O0 -g2

ex1: ex1_objs 
	$(CC) $(EX1_OBJS) $(CFLAGS) -L../.. -lxoc -o ex1.exe -lstdc++ -lm
	@echo "success!!"

INC=-I .
%.o:%.cpp
	@echo "build $<"
	$(CC) $(CFLAGS) $(INC) -c $< -o $@

ex1_objs: $(EX1_OBJS)
