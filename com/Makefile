include ./Makefile.inc

ifeq ($(CFLAGS),)
CFLAGS = \
    -D_DEBUG_ \
    -DFOR_DEX \
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

libxcom: com_objs 
	ar rvs libxcom.a $(COM_OBJS)
	@echo "success!!"

%.o:%.cpp
	@echo "build $<"
	$(INFER) gcc $(CFLAGS) -c $< -o $@

com_objs: $(COM_OBJS)

clean:
	@find ./ -name "*.a" | xargs rm -f
	@find ./ -name "*.o" | xargs rm -f
	@find ./ -name "*.tmp" | xargs rm -f
