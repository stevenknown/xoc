#!/usr/bin/env python
import sys, os, stat, re

fileTypes = ["c", "h", "cpp", "txt", "impl", "cc", "desc", "js"]
anchorFile="LOGLOG"

def help():
    print("============TO tranformation tab to Four Spacebar=======================")
    print("./rebase : Do the tranformation recursivly, the start path is current dir")
    print("./rebase PATH: Do the tranformation recursivly, the start path is PATH")
    print("./rebase --help: To show this help")

def walk(path):
       for dirpath,dirnames,filenames in os.walk(path):
           for file in filenames:
                t = file.split(".")[-1]
                if(t in fileTypes):
                   absFile = os.path.join(dirpath, file);
                   yield absFile

def retab(path, First):
    anchorTime = os.stat(anchorFile).st_mtime;

    for file in walk(path):
        if(First == False):
            if(os.stat(file).st_mtime <= anchorTime):
                continue;

        print (file);
        buff = ""
        fp = open(file, "r");
        for line in fp:
            tline = ""
            tline = line.replace("\t", "    ");
            tline = tline.rstrip() + "\n";
            buff = buff + tline;

        fp.close();

        fp = open(file, "w");
        fp.write(buff);
        fp.close();

    #update the anchor file
    fp = open(anchorFile, "w");
    fp.close();

def replaceFile(file):
    print (file);
    buff = ""
    fp = open(file, "r");
    for line in fp:
        tline = ""
        tline = line.replace("\t", "    ");
        tline = tline.rstrip() + "\n";
        buff = buff + tline;

    fp.close();

    fp = open(file, "w");
    fp.write(buff);
    fp.close();

def getPath():
    if len(sys.argv) == 1:
        path = os.path.abspath(os.getcwd());
    else:
        if(sys.argv[1] == "--help"):
            help();
            exit();

        argPath = os.path.abspath(sys.argv[1])
        if(os.path.exists(argPath)):
            path = sys.argv[1];
        else:
            print("Path %s do not exsits!" % argPath )
            exit();

    if(os.path.exists(anchorFile) == False):
        fp = open(anchorFile, "w");
        fp.close();

    return path

if __name__ == '__main__':
    retab(getPath(), True);
    #if(os.path.exists(anchorFile) == False):
    #    retab(getPath(), True);
    #else:
    #    retab(getPath(), True); #False); #inhibit the 'anchorfile' policy because it make script behaved weired.
