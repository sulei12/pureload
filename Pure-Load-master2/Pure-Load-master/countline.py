# -*- coding: utf-8 -*-
#!/usr/bin/python
# linecount.py
# 2009-1-20
# author:
#   Jason Lee
#
import sys
import os

exts = ['.f90','.for','.f','.m']
def read_line_count(fname):
    count = sum(1 for line in open(fname))
    return count+1

if __name__ == '__main__':

    count = 0
    fcount = 0
    for root,dirs,files in os.walk(os.getcwd()):
        for f in files:
            # Check the sub directorys
            fname = (root + '/'+ f).lower()
            try:
                ext = f[f.rindex('.'):]
            except:
                print (fname)
            try:
                if(exts.index(ext) >= 0):
                    fcount += 1
                    c = read_line_count(fname)
                    count += c
            except:
                pass

    print ('Number of source code files:',fcount)
    print ('Number of source code lines:',count)
