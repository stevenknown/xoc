MISC_TARG_MACH_ELF_OBJS+=\
../arm/arm_elf_targinfo.o\
../score/score_elf_targinfo.o\
../x64/x64_elf_targinfo.o\
../x86/x86_elf_targinfo.o
../teco/arm_elf_targinfo.o

ELF_OBJS+=\
elf_mgr.o\
misc_elf_mgr.o\
elf_header.o\
elf_targinfo.o\
$(MISC_TARG_MACH_ELF_OBJS)

CFLAGS+=-Wno-unknown-pragmas
