#!/usr/bin/env python
import sys, os, stat, re

def replaceFile(file, num_of_line):
    buff = ""
    fp = open(file, "r");
    
    cnt = 0;
    for line in fp:
        if (cnt < num_of_line):
            tmp = "";
        else:
            buff = buff + line;
        cnt = cnt + 1;
    fp.close();

    #Override file.
    fp = open(file, "w");
    fp.write(buff);
    fp.close();

if __name__ == '__main__':
    file = "";
    num_of_line = 0;
    argn = 0;
    for arg in sys.argv:
        if (argn == 1):
            file = arg;
        elif (argn == 2):
            num_of_line = int(arg);
        argn = argn + 1;
    replaceFile(file, num_of_line);
