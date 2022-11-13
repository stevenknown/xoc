CC := $(shell which clang++ > /dev/null)
ifndef CC
  CC = $(if $(shell which clang), clang, gcc)
endif

EX1_OBJS+=ex1.o

CFLAGS=-DFOR_DEX -D_DEBUG_ -O0 -g2 -D_SUPPORT_C11_

ex1: ex1_objs 
	$(CC) $(EX1_OBJS) $(CFLAGS) -L../.. -lxoc -L../../com -lxcom -o ex1.exe -lstdc++ -lm
	@echo "SUCCESS!!"

INC=-I .
%.o:%.cpp
	@echo "BUILD $<"
	$(CC) $(CFLAGS) $(INC) -c $< -o $@

ex1_objs: $(EX1_OBJS)

clean:
	@find ./ -name "*.o" | xargs rm -f
	@find ./ -name "*.exe" | xargs rm -f
	@find ./ -name "*.gr" | xargs rm -f
	@find ./ -name "*.tmp" | xargs rm -f
