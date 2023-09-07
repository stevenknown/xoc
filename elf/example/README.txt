readelf.exe
=======

readelf.exe can read ELF information from binary and dump to screen or other output file.
As a demo, readelf.exe will write the ELF information into a new ELF file.

Build readelf.exe:
    # Add targ macro and set targ macro to 'true' to enable the function.
    # E.g:only enable the readelf to arm and x86 target.
    make -f Makefile.readelf DEBUG=true FOR_ARM=true FOR_X86=true

Run readelf.exe:
    ./readelf.exe a.elf
    ./readelf.exe a.elf a.dump
