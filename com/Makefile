ifeq ($(TARG),)
  TARG=NO_TARG
endif

include ./Makefile.inc
include Makefile.option

#SET DEFAULT CFLAGS.
CFLAGS+=\
  -D$(TARG) \
  -D_SUPPORT_C11_ \
  -Wno-unknown-pragmas \
  -Wno-write-strings \
  -Wsign-promo \
  -Wparentheses \
  -Wformat \
  -Wsign-compare \
  -Wpointer-arith \
  -Wno-multichar \
  -Winit-self \
  -Wswitch

#Disable -Wconversion to avoid too much complaints.
ifneq (,$(filter $(CC),g++ gcc))
  CFLAGS+=-Wno-strict-aliasing -finline-limit=10000000
endif

$(COM_OUTPUT): com_objs 
	ar rvs $(COM_OUTPUT) $(COM_OBJS)
	@echo "SUCCESS!!"

%.o:%.cpp
	@echo "BUILD $<"
	$(INFER) $(CC) $(CFLAGS) -c $< -o $@

com_objs: $(COM_OBJS)

clean:
	@find ./ -name "*.a" | xargs rm -f
	@find ./ -name "*.o" | xargs rm -f
	@find ./ -name "*.tmp" | xargs rm -f
