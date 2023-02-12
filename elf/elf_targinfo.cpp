/*@
Copyright (c) 2013-2021, Su Zhenyu steven.known@gmail.com
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.
    * Redistributions in binary form must reproduce the above copyright
      notice, this list of conditions and the following disclaimer in the
      documentation and/or other materials provided with the distribution.
    * Neither the name of the Su Zhenyu nor the names of its contributors
      may be used to endorse or promote products derived from this software
      without specific prior written permission.

THIS SOFTWARE IS PROVIDED "AS IS" AND ANY
EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE
USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
@*/
#include "elfinc.h"

namespace elf {

typedef struct {
    Word machtype;
    CHAR const* name;
} MachType;

static MachType g_machtype[] = {
    { EM_NONE, "No machine", },
    { EM_M32, "AT&T WE 32100", },
    { EM_SPARC, "SPARC", },
    { EM_386, "Intel 386", },
    { EM_68K, "Motorola 68000", },
    { EM_88K, "Motorola 88000", },
    { EM_IAMCU, "Intel MCU", },
    { EM_860, "Intel 80860", },
    { EM_MIPS, "MIPS R3000", },
    { EM_S370, "IBM System/370", },
    { EM_MIPS_RS3_LE, "MIPS RS3000 Little-endian", },
    { EM_PARISC, "Hewlett-Packard PA-RISC", },
    { EM_VPP500, "Fujitsu VPP500", },
    { EM_SPARC32PLUS, "Enhanced instruction set SPARC", },
    { EM_960, "Intel 80960", },
    { EM_PPC, "PowerPC", },
    { EM_PPC64, "PowerPC64", },
    { EM_S390, "IBM System/390", },
    { EM_SPU, "IBM SPU/SPC", },
    { EM_V800, "NEC V800", },
    { EM_FR20, "Fujitsu FR20", },
    { EM_RH32, "TRW RH-32", },
    { EM_RCE, "Motorola RCE", },
    { EM_ARM, "ARM", },
    { EM_ALPHA, "DEC Alpha", },
    { EM_SH, "Hitachi SH", },
    { EM_SPARCV9, "SPARC V9", },
    { EM_TRICORE, "Siemens TriCore", },
    { EM_ARC, "Argonaut RISC Core", },
    { EM_H8_300, "Hitachi H8/300", },
    { EM_H8_300H, "Hitachi H8/300H", },
    { EM_H8S, "Hitachi H8S", },
    { EM_H8_500, "Hitachi H8/500", },
    { EM_IA_64, "Intel IA-64 processor architecture", },
    { EM_MIPS_X, "Stanford MIPS-X", },
    { EM_COLDFIRE, "Motorola ColdFire", },
    { EM_68HC12, "Motorola M68HC12", },
    { EM_MMA, "Fujitsu MMA Multimedia Accelerator", },
    { EM_PCP, "Siemens PCP", },
    { EM_NCPU, "Sony nCPU embedded RISC processor", },
    { EM_NDR1, "Denso NDR1 microprocessor", },
    { EM_STARCORE, "Motorola Star*Core processor", },
    { EM_ME16, "Toyota ME16 processor", },
    { EM_ST100, "STMicroelectronics ST100 processor", },
    { EM_TINYJ, "Advanced Logic Corp. TinyJ embedded processor family", },
    { EM_X86_64, "AMD x86-64 architecture", },
    { EM_PDSP, "Sony DSP Processor", },
    { EM_PDP10, "Digital Equipment Corp. PDP-10", },
    { EM_PDP11, "Digital Equipment Corp. PDP-11", },
    { EM_FX66, "Siemens FX66 microcontroller", },
    { EM_ST9PLUS, "STMicroelectronics ST9+ 8/16 bit microcontroller", },
    { EM_ST7, "STMicroelectronics ST7 8-bit microcontroller", },
    { EM_68HC16, "Motorola MC68HC16 Microcontroller", },
    { EM_68HC11, "Motorola MC68HC11 Microcontroller", },
    { EM_68HC08, "Motorola MC68HC08 Microcontroller", },
    { EM_68HC05, "Motorola MC68HC05 Microcontroller", },
    { EM_SVX, "Silicon Graphics SVx", },
    { EM_ST19, "STMicroelectronics ST19 8-bit microcontroller", },
    { EM_VAX, "Digital VAX", },
    { EM_CRIS, "Axis Communications 32-bit embedded processor", },
    { EM_JAVELIN, "Infineon Technologies 32-bit embedded processor", },
    { EM_FIREPATH, "Element 14 64-bit DSP Processor", },
    { EM_ZSP, "LSI Logic 16-bit DSP Processor", },
    { EM_MMIX, "Donald Knuth's educational 64-bit processor", },
    { EM_HUANY, "Harvard University machine-independent object files", },
    { EM_PRISM, "SiTera Prism", },
    { EM_AVR, "Atmel AVR 8-bit microcontroller", },
    { EM_FR30, "Fujitsu FR30", },
    { EM_D10V, "Mitsubishi D10V", },
    { EM_D30V, "Mitsubishi D30V", },
    { EM_V850, "NEC v850", },
    { EM_M32R, "Mitsubishi M32R", },
    { EM_MN10300, "Matsushita MN10300", },
    { EM_MN10200, "Matsushita MN10200", },
    { EM_PJ, "picoJava", },
    { EM_OPENRISC, "OpenRISC 32-bit embedded processor", },
    { EM_ARC_COMPACT, "ARC International ARCompact processor (old spelling/synonym: EM_ARC_A5)", },
    { EM_XTENSA, "Tensilica Xtensa Architecture", },
    { EM_VIDEOCORE, "Alphamosaic VideoCore processor", },
    { EM_TMM_GPP, "Thompson Multimedia General Purpose Processor", },
    { EM_NS32K, "National Semiconductor 32000 series", },
    { EM_TPC, "Tenor Network TPC processor", },
    { EM_SNP1K, "Trebia SNP 1000 processor", },
    { EM_ST200, "STMicroelectronics (www.st.com) ST200", },
    { EM_IP2K, "Ubicom IP2xxx microcontroller family", },
    { EM_MAX, "MAX Processor", },
    { EM_CR, "National Semiconductor CompactRISC microprocessor", },
    { EM_F2MC16, "Fujitsu F2MC16", },
    { EM_MSP430, "Texas Instruments embedded microcontroller msp430", },
    { EM_BLACKFIN, "Analog Devices Blackfin (DSP) processor", },
    { EM_SE_C33, "S1C33 Family of Seiko Epson processors", },
    { EM_SEP, "Sharp embedded microprocessor", },
    { EM_ARCA, "Arca RISC Microprocessor", },
    { EM_UNICORE, "Microprocessor series from PKU-Unity Ltd. and MPRC of Peking University", },
    { EM_EXCESS, "eXcess: 16/32/64-bit configurable embedded CPU", },
    { EM_DXP, "Icera Semiconductor Inc. Deep Execution Processor", },
    { EM_ALTERA_NIOS2, "Altera Nios II soft-core processor", },
    { EM_CRX, "National Semiconductor CompactRISC CRX", },
    { EM_XGATE, "Motorola XGATE embedded processor", },
    { EM_C166, "Infineon C16x/XC16x processor", },
    { EM_M16C, "Renesas M16C series microprocessors", },
    { EM_DSPIC30F, "Microchip Technology dsPIC30F Digital Signal Controller", },
    { EM_CE, "Freescale Communication Engine RISC core", },
    { EM_M32C, "Renesas M32C series microprocessors", },
    { EM_TSK3000, "Altium TSK3000 core", },
    { EM_RS08, "Freescale RS08 embedded processor", },
    { EM_SHARC, "Analog Devices SHARC family of 32-bit DSP processors", },
    { EM_ECOG2, "Cyan Technology eCOG2 microprocessor", },
    { EM_SCORE7, "Sunplus S+core7 RISC processor", },
    { EM_DSP24, "New Japan Radio (NJR) 24-bit DSP Processor", },
    { EM_VIDEOCORE3, "Broadcom VideoCore III processor", },
    { EM_LATTICEMICO32, "RISC processor for Lattice FPGA architecture", },
    { EM_SE_C17, "Seiko Epson C17 family", },
    { EM_TI_C6000, "The Texas Instruments TMS320C6000 DSP family", },
    { EM_TI_C2000, "The Texas Instruments TMS320C2000 DSP family", },
    { EM_TI_C5500, "The Texas Instruments TMS320C55x DSP family", },
    { EM_MMDSP_PLUS, "STMicroelectronics 64bit VLIW Data Signal Processor", },
    { EM_CYPRESS_M8C, "Cypress M8C microprocessor", },
    { EM_R32C, "Renesas R32C series microprocessors", },
    { EM_TRIMEDIA, "NXP Semiconductors TriMedia architecture family", },
    { EM_HEXAGON, "Qualcomm Hexagon processor", },
    { EM_8051, "Intel 8051 and variants", },
    { EM_STXP7X, "STMicroelectronics STxP7x family of configurable and extensible RISC processors", },
    { EM_NDS32, "Andes Technology compact code size embedded RISC processor family", },
    { EM_ECOG1, "Cyan Technology eCOG1X family", },
    { EM_ECOG1X, "Cyan Technology eCOG1X family", },
    { EM_MAXQ30, "Dallas Semiconductor MAXQ30 Core Micro-controllers", },
    { EM_XIMO16, "New Japan Radio (NJR) 16-bit DSP Processor", },
    { EM_MANIK, "M2000 Reconfigurable RISC Microprocessor", },
    { EM_CRAYNV2, "Cray Inc. NV2 vector architecture", },
    { EM_RX, "Renesas RX family", },
    { EM_METAG, "Imagination Technologies META processor architecture", },
    { EM_MCST_ELBRUS, "MCST Elbrus general purpose hardware architecture", },
    { EM_ECOG16, "Cyan Technology eCOG16 family", },
    { EM_CR16, "National Semiconductor CompactRISC CR16 16-bit microprocessor", },
    { EM_ETPU, "Freescale Extended Time Processing Unit", },
    { EM_SLE9X, "Infineon Technologies SLE9X core", },
    { EM_L10M, "Intel L10M", },
    { EM_K10M, "Intel K10M", },
    { EM_AARCH64, "ARM AArch64", },
    { EM_AVR32, "Atmel Corporation 32-bit microprocessor family", },
    { EM_STM8, "STMicroeletronics STM8 8-bit microcontroller", },
    { EM_TILE64, "Tilera TILE64 multicore architecture family", },
    { EM_TILEPRO, "Tilera TILEPro multicore architecture family", },
    { EM_MICROBLAZE, "Xilinx MicroBlaze 32-bit RISC soft processor core", },
    { EM_CUDA, "NVIDIA CUDA architecture", },
    { EM_TILEGX, "Tilera TILE-Gx multicore architecture family", },
    { EM_CLOUDSHIELD, "CloudShield architecture family", },
    { EM_COREA_1ST, "KIPO-KAIST Core-A 1st generation processor family", },
    { EM_COREA_2ND, "KIPO-KAIST Core-A 2nd generation processor family", },
    { EM_ARC_COMPACT2, "Synopsys ARCompact V2", },
    { EM_OPEN8, "Open8 8-bit RISC soft processor core", },
    { EM_RL78, "Renesas RL78 family", },
    { EM_VIDEOCORE5, "Broadcom VideoCore V processor", },
    { EM_78KOR, "Renesas 78KOR family", },
    { EM_56800EX, "Freescale 56800EX Digital Signal Controller (DSC)", },
    { EM_BA1, "Beyond BA1 CPU architecture", },
    { EM_BA2, "Beyond BA2 CPU architecture", },
    { EM_XCORE, "XMOS xCORE processor family", },
    { EM_MCHP_PIC, "Microchip 8-bit PIC(r) family", },
    { EM_INTEL205, "Reserved by Intel", },
    { EM_INTEL206, "Reserved by Intel", },
    { EM_INTEL207, "Reserved by Intel", },
    { EM_INTEL208, "Reserved by Intel", },
    { EM_INTEL209, "Reserved by Intel", },
    { EM_KM32, "KM211 KM32 32-bit processor", },
    { EM_KMX32, "KM211 KMX32 32-bit processor", },
    { EM_KMX16, "KM211 KMX16 16-bit processor", },
    { EM_KMX8, "KM211 KMX8 8-bit processor", },
    { EM_KVARC, "KM211 KVARC processor", },
    { EM_CDP, "Paneve CDP architecture family", },
    { EM_COGE, "Cognitive Smart Memory Processor", },
    { EM_COOL, "iCelero CoolEngine", },
    { EM_NORC, "Nanoradio Optimized RISC", },
    { EM_CSR_KALIMBA, "CSR Kalimba architecture family", },
    { EM_AMDGPU, "AMD GPU architecture", },
    { EM_RISCV, "RISC-V", },
    { EM_LANAI, "Lanai 32-bit processor", },
    { EM_BPF, "Linux kernel bpf virtual machine", },
    { EM_VE, "NEC SX-Aurora VE", },
    { EM_CSKY, "C-SKY 32-bit processor", },
};
static UINT g_machtype_num = sizeof(g_machtype) / sizeof(g_machtype[0]);

CHAR const* ELFTargInfo::getMachineTypeName() const
{
    ASSERTN(m_elf_mgr->getHdr().e_machine < g_machtype_num,
            ("Target Dependent Code"));
    return g_machtype[m_elf_mgr->getHdr().e_machine].name;
}


//Return the relocation type.
CHAR const* ELFTargInfo::getRelTypeName(Word r) const
{
    ASSERTN(0, ("Target Dependent Code"));
    return nullptr;
}

} //namespace elf
