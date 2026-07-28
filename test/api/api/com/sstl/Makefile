$(info "==---- START TEST CASE GENERATION ----==")

ifeq ($(THREAD_NUM),)
  THREAD_NUM:=1
endif

ifeq ($(CC),)
  $(error "MISS CC")
endif

ifeq ($(TARG),)
  $(error "NEED TARG INFO:e.g:make TARG=FOR_ARM TARG_DIR=../arm")
endif

ifeq ($(TARG_DIR),)
  $(error "NEED TARG_DIR INFO:e.g:make TARG=FOR_ARM TARG_DIR=../arm")
endif

#override ROOT_DIR:=$(CURDIR)/../../../../src

GEN_OBJS+=test.o
COM_DIR:=$(ROOT_DIR)/com
TARG_DIR_NAME:=$(notdir $(TARG_DIR))
TEST_EXE:=test.exe
CURFILE:=$(CURDIR)/Makefile

ifeq ($(wildcard $(ROOT_DIR)),)
  $(error "$(ROOT_DIR) SEEMS NOT VALID DIR.")
else ifeq ($(wildcard $(COM_DIR)),)
  $(error "$(COM_DIR) SEEMS NOT VALID DIR.")
endif

include $(COM_DIR)/Makefile.inc
include $(ROOT_DIR)/Makefile.option

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
$(info "REF_TARGMACH_INFO:$(REF_TARGMACH_INFO)")

.PHONY: build_gen_objs

$(TEST_EXE): $(COM_OUTPUT)
	@echo "IN MAKEFILE: START BUILD $(TEST_EXE)"
	@echo "EXEC:"
	$(CC) -std=c++0x \
      $(CFLAGS) \
      -o $@ \
      -D$(TARG) \
      -Xlinker "-(" \
      $(GEN_OBJS) \
      $(COM_DIR)/$(COM_OUTPUT) \
      -lstdc++ -lm \
      -Xlinker "-)"
	@echo "IN MAKEFILE: END BUILD $(TEST_EXE)"
	@echo "SUCCESS TO GENERATE $(TEST_EXE)!!"

build_gen_objs: pre_build_gen_objs $(GEN_OBJS)
	@echo "SUCCESS TO GENERATE: $(GEN_OBJS)"

pre_build_gen_objs:
	@echo "START BUILD: $(GEN_OBJS)"

$(COM_OUTPUT): build_gen_objs
	@echo "THERE IS NO NEED TO REBUILD $(COM_OUTPUT)"
#NOTE:There is no need to rebuild libxcom.a
#$(COM_OUTPUT): build_gen_objs
#	@echo "IN MAKEFILE: START BUILD $(COM_OUTPUT)"
#	@echo "EXEC:"
#	make -C $(COM_DIR) \
#      -j$(THREAD_NUM) \
#      -f Makefile \
#      TARG=$(TARG) \
#      TARG_DIR=$(TARG_DIR) \
#      ROOT_DIR=$(ROOT_DIR) \
#      DEBUG=$(DEBUG) \
#      CFLAGS="$(CFLAGS)" \
#      THREAD_NUM=$(THREAD_NUM) \
#      REF_TARGMACH_INFO=$(REF_TARGMACH_INFO)
#	@echo "IN MAKEFILE: END BUILD $(COM_OUTPUT)"

%.o:%.cpp
	@echo "BUILD $<"
	@echo "EXEC:"
	$(INFER) $(CC) -D$(TARG) $(CFLAGS) -c $< -o $@

clean:
	@find ./ -name "*.exe" | xargs rm -f
	@find ./ -name "*.gcda" | xargs rm -f
	@find ./ -name "*.gcno" | xargs rm -f
	@find ./ -name "*.o" | xargs rm -f
	@find ./ -name "*.d" | xargs rm -f
	@find ./ -name "*.dot" | xargs rm -f
	@find ./ -name "*.elf" | xargs rm -f
	@find ./ -name "*.out" | xargs rm -f
	@find ./ -name "*.tmp" | xargs rm -f
	@find ./ -name "*.vcg" | xargs rm -f
	@find ./ -name "*.cxx" | xargs rm -f
	@find ./ -name "*.asm" | xargs rm -f
	@find ./ -name "*.log" | xargs rm -f

$(info "==---- END TEST CASE GENERATION ----==")
