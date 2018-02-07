OBJS +=\
ltype.o \
xmat.o \
comf.o \
smempool.o \
agraph.o \
sgraph.o \
rational.o \
testbs.o \
flty.o \
strbuf.o \
linsys.o \
bs.o

ifeq ($(CFLAGS),)
CFLAGS = \
    -D_DEBUG_ \
	-Wno-write-strings \
	-Wsign-promo \
	-Wparentheses \
    -Wformat \
	-Wsign-compare \
	-Wpointer-arith \
	-Wno-multichar \
	-Winit-self \
	-Wuninitialized \
    -Wstrict-aliasing=3 \
	-Wswitch \
	-O2 \
	-g2 \
	-Wall

#CFLAGS+=\
	-Werror=pointer-to-int-cast \
	-Wmaybe-uninitialized \
	-Wtype-limits \
	-Wstrict-overflow \
	-finline-limit=10000000
endif

all: objs 
	ar rvs libxcom.a $(OBJS)
	@echo "success!!"

%.o:%.cpp
	@echo "build $<"
	$(INFER) gcc $(CFLAGS) -c $< -o $@

objs: $(OBJS)

clean:
	@find ./ -name "*.a" | xargs rm -f
	@find ./ -name "*.o" | xargs rm -f
	@find ./ -name "*.tmp" | xargs rm -f
