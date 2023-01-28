readelf.exe
=======

readelf.exe can read ELF information from binary and dump to screen or other output file.
As a demo, readelf.exe will write the ELF information into a new ELF file.

Build readelf.exe:
    make -f Makefile.readelf

Run readelf.exe:
    ./readelf.exe a.elf
    ./readelf.exe a.elf a.dump
