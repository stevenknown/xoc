$(info "====---- START Makefile.xcom ----====")

ifeq ($(TARG),)
  TARG:=NO_TARG
endif

override ROOT_DIR:=$(CURDIR)/..

COM_DIR:=$(ROOT_DIR)/com

#Check wether each directory is valid.
ifeq ($(wildcard $(ROOT_DIR)),)
  $(error "$(ROOT_DIR) SEEMS NOT VALID DIR.")
else ifeq ($(wildcard $(COM_DIR)),)
  $(error "$(COM_DIR) SEEMS NOT VALID DIR.")
endif

include $(COM_DIR)/Makefile.inc
include $(COM_DIR)/Makefile.option

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
$(info "ROOT_DIR:$(ROOT_DIR)")
$(info "CC:$(CC)")
$(info "INC:$(INC)")
$(info "CFLAGS:$(CFLAGS)")
$(info "DEBUG:$(DEBUG)")
$(info "THREAD_NUM:$(THREAD_NUM)")

$(COM_OUTPUT): build_com_objs
	@echo "IN COM MAKEFILE: START BUILD $(COM_OUTPUT)!!"
	@if [ ! -f "$@" ] || [ "$<" -nt "$@" ]; then \
      echo "EXEC:"; \
      ar -rvscT $(COM_OUTPUT) \
        $(COM_OBJS); \
      echo "SUCCESS TO GENERATE $(COM_OUTPUT)!!"; \
    else \
      echo "$(COM_OUTPUT) ALREADY EXISTS!!"; \
    fi

print_time = $(shell stat --format=%y "$1")

#COMPILE CPP WITH TIME-STAMP PRINTING
#%.o:%.cpp
#	@echo "BUILD $<"
#	@echo "($<) last modified time: $(call print_time,$<)"
#	@if [ -f "$(COM_DIR)/$@" ]; then \
#       echo "$(COM_DIR)/$@ last modified time: \
#             $(call print_time,$(COM_DIR)/$@)"; \
#    fi
#	@if [ ! -f "$(COM_DIR)/$@" ]; then \
#      echo "EXEC:THE FIRST BUILD:$(INFER) $(CC) $(CFLAGS) -c $< -o $@;"; \
#      $(INFER) $(CC) $(CFLAGS) -c $< -o $@; \
#    elif [ "$<" -nt "$@" ]; then \
#      echo "EXEC:OUT OF DATE:$(INFER) $(CC) $(CFLAGS) -c $< -o $@;"; \
#      $(INFER) $(CC) $(CFLAGS) -c $< -o $@; \
#    fi

%.o:%.cpp
	@echo "BUILD $<"
	@if [ ! -f "$(COM_DIR)/$@" ]; then \
       echo "EXEC:THE FIRST BUILD:$(INFER) $(CC) $(CFLAGS) -c $< -o $@;"; \
       $(INFER) $(CC) $(CFLAGS) -c $< -o $@; \
     elif [ "$<" -nt "$@" ]; then \
       echo "EXEC:OUT OF DATE:$(INFER) $(CC) $(CFLAGS) -c $< -o $@;"; \
       $(INFER) $(CC) $(CFLAGS) -c $< -o $@; \
     fi

build_com_objs: pre_build_com_objs $(COM_OBJS)
	@echo "SUCCESS TO GENERATE: $(COM_OBJS)"

pre_build_com_objs:
	@echo "START BUILD: $(COM_OBJS)"

clean:
	@find ./ -name "*.a" | xargs rm -f
	@find ./ -name "*.d" | xargs rm -f
	@find ./ -name "*.o" | xargs rm -f
	@find ./ -name "*.tmp" | xargs rm -f

-include $(COM_DIR)/*.d

$(info "====---- END Makefile.xcom ----====")
