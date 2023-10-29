$(info "====---- START Makefile.xcom ----====")

ifeq ($(TARG),)
  TARG=NO_TARG
endif

COM_DIR=.

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

#Display internal variables.
$(info "WIN=$(WIN),WSL=$(WSL)")
$(info "TARG:$(TARG)")
$(info "TARG_DIR:$(TARG_DIR)")
$(info "CC:$(CC)")
$(info "INC:$(INC)")
$(info "CFLAGS:$(CFLAGS)")
$(info "DEBUG:$(DEBUG)")
$(info "THREAD_NUM:$(THREAD_NUM)")

$(COM_OUTPUT): com_objs 
	@echo "BUILD $(COM_OUTPUT)!!"
	@echo "EXEC:"
	ar rvs $(COM_OUTPUT) $(COM_OBJS)
	@echo "SUCCESS TO GENERATE $(COM_OUTPUT)!!"

%.o:%.cpp
	@echo "BUILD $<"
	@echo "EXEC:"
	$(INFER) $(CC) $(CFLAGS) -c $< -o $@

com_objs: $(COM_OBJS)
	@echo "EXEC:com_objs: $(COM_OBJS)"

clean:
	@find ./ -name "*.a" | xargs rm -f
	@find ./ -name "*.d" | xargs rm -f
	@find ./ -name "*.o" | xargs rm -f
	@find ./ -name "*.tmp" | xargs rm -f

-include $(COM_DIR)/*.d

$(info "====---- END Makefile.xcom ----====")
