#!/usr/bin/python

from __future__ import print_function

import os
from os.path import splitext, join
import subprocess
import sys
import time

TEST_EXT = '.ml'
BASELINE_EXT = '.out'
FLAGS = ['-nosugar', '-data', '-noincomplete-warning']

def find_tests(root):
    tests = []
    for path, dirs, files in os.walk(root):
        files = [(f[0], f[1]) for f in [splitext(f) for f in files]]
        tests.extend([(path, f[0]) for f in files if f[1] == TEST_EXT])
    return tests

def gather_data(prog, path, base):
    print(subprocess.check_output([prog] + FLAGS + [join(path, base + TEST_EXT)], stderr=subprocess.STDOUT), end='')

def print_usage(args):
    print("Usage: {0} <prog> <test|testdir>".format(args[0]))

def main(args):
    if len(args) == 3:
        prog = args[1]
        path = args[2]
        if not os.path.exists(prog):
            print_usage(args)
        elif os.path.exists(path) and os.path.isdir(path):
            for path, base in find_tests(path):
                gather_data(prog, path, base)
        else:
            path, filename = os.path.split(path)
            base, ext = splitext(filename)
            if ext != TEST_EXT:
                print_usage(args)
            else:
                gather_data(prog, path, base)
    else:
        print_usage(args)

if __name__ == '__main__':
    main(sys.argv)
