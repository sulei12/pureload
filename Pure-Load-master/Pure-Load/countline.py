# -*- coding: utf-8 -*-
#!/usr/bin/python
# linecount.py
# 2009-1-20
# author:
#   Jason Lee
# modified by DS
import os

exts = ['.f90','.for','.f','.m']

def read_line_count(fname):
    with open(fname,'r') as f:
        count = sum(1 for line in f)
    return count+1

if __name__ == '__main__':
    count, fcount = 0, 0
    for root,dirs,files in os.walk(os.getcwd()):
        for f in files:
            ext = os.path.splitext(f)[1]
            if ext in exts:
                fname = os.path.join(root,f)
                count += read_line_count(fname)
                fcount += 1
    print ('Number of source code files:',fcount)
    print ('Number of source code lines:',count)
