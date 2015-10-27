COM_OBJS +=\
ltype.o \
xmat.o \
comf.o \
smempool.o \
agraph.o \
sgraph.o \
rational.o \
testbs.o \
flty.o \
linsys.o \
bs.o

CFLAGS = -DFOR_PAC  -D_LINUX_ -Wno-write-strings -Wsign-promo -Werror=pointer-to-int-cast -Wparentheses \
   -Wformat -Wsign-compare -Wpointer-arith -Wno-multichar -Winit-self -Wuninitialized -Wmaybe-uninitialized -Wtype-limits -Wstrict-overflow \
   -Wstrict-aliasing=3 -finline-limit=10000000 -Wswitch -O3 -g2 -Wall #-fprofile-arcs -ftest-coverage #-D_DEBUG_ # 

INC=.
%.o:%.cpp
	@echo "build $<"
	$(INFER) gcc $(CFLAGS) $(INC) -c $< -o $@

com_objs: $(COM_OBJS)

all: com_objs 
	ar rvs libxcom.a $(COM_OBJS)
	@echo "success!!"

clean:
	@find -name "*.a" | xargs rm -f
	@find -name "*.o" | xargs rm -f
	@find -name "*.tmp" | xargs rm -f
	@find -name "tags" | xargs rm -f

