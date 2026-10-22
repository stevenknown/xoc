CC := $(shell which clang++ > /dev/null)
ifndef CC
  CC = $(if $(shell which clang), clang, gcc)
endif

OBJS+=main.o

CFLAGS=-DFOR_DEX -D_DEBUG_ -O0 -g2 -D_SUPPORT_C11_

grreader: objs
	$(CC) $(OBJS) $(CFLAGS) -L../.. -lxoc -L../../com -lxcom -o \
      grreader.exe -lstdc++ -lm
	@echo "SUCCESS!!"

INC=-I .
%.o:%.cpp
	@echo "BUILD $<"
	$(CC) $(CFLAGS) $(INC) -c $< -o $@

objs: $(OBJS)

clean:
	@find ./ -name "*.o" | xargs rm -f
	@find ./ -name "*.exe" | xargs rm -f
	@find ./ -name "*.gr" | xargs rm -f
	@find ./ -name "*.tmp" | xargs rm -f
	@find ./ -name "*.log" | xargs rm -f
