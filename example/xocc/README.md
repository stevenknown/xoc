This example implements a C compiler named xocc. It requires both the xgen and xocfe
modules to participate in the XOC compilation process to build the xocc compiler.
The compilation steps are as follows:

1. First, download the xgen and xocfe modules.
```cmd
    git clone https://github.com/stevenknown/xoc.git
    git clone https://github.com/stevenknown/xocfe.git
```

2. Copy the C frontend xocfe folder located under xocfe\src to the root directory of xoc.
```cmd
    cd xoc
    cp -rf <THE-PATH-OF-XOCFE>/xocfe/src/cfe ./
```

3. Copy the entire xgen, xocc and arm directory to the root directory of xoc.
```cmd
    cd xoc
    cp -rf <THE-PATH-OF-XOCFE>/xgen/arm ./
    cp -rf <THE-PATH-OF-XOCFE>/xgen/xgen ./
    cp -rf <THE-PATH-OF-XOCFE>/xgen/xocc ./
```

4. Navigate into the xocc directory and run build_xocc.sh.
```cmd
    cd xoc/xocc
    ./build_xocc.sh
```

5. There are plenty of test files under the `xoc/test` directory. Try compiling `xoc/test/hello.c`
with the built `xocc.exe`; this will generate assembly code targeting the ARM architecture.
```cmd
    cd xoc
    xocc/xocc.exe test/compile/hello.c -O3 -o hello.s
```

6. Install arm-assembler, arm-linker:    
```cmd
    sudo apt-get install gcc-arm-linux-gnueabihf    
```

7. Install qemu-arm:    
```cmd
    sudo apt-get install qemu-user-static       
```
    or
```cmd
    sudo apt-get install qemu-user-binfmt
```

8. Run test
```cmd
    arm-linux-gnueabihf-as hello.s -o hello.o
    arm-linux-gnueabihf-gcc hello.o -o hello.out -static
    qemu-arm -L /usr/arm-linux-gnueabihf hello.out
```
