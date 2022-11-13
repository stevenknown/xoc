include ./Makefile.inc

ifeq ($(TARG),)
  TARG=NO_TARG
endif

ifeq ($(CC),)
  CC:=$(shell which clang++ > /dev/null)
  ifndef CC
    CC=$(if $(shell which clang),clang,gcc)
  endif
endif

ifeq ($(CFLAGS),)
  CFLAGS+=\
    -D$(TARG) \
    -D_SUPPORT_C11_ \
    -O2 \
    -g2 \
    -Wall \
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
endif

ifdef WIN
  #Add predefined macro if build host is Windows.
  CFLAGS+=-D_ON_WINDOWS_
endif

ifeq ($(DEBUG), true)
  CFLAGS+=-D_DEBUG_
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
