#!/usr/bin/python

from __future__ import print_function

import os
from os.path import splitext, join
import subprocess
import sys
import time

TEST_EXT = '.ml'
BASELINE_EXT = '.out'
FLAGS = ['-nosugar', '-noincomplete-warning']

def find_tests(root):
    tests = []
    for path, dirs, files in os.walk(root):
        files = [(f[0], f[1]) for f in [splitext(f) for f in files]]
        tests.extend([(path, f[0]) for f in files if f[1] == TEST_EXT])
    return tests

def run_test(prog, path, base):
    return subprocess.check_output([prog] + FLAGS + [join(path, base + TEST_EXT)], stderr=subprocess.STDOUT)

def gen_timestamp():
    return "(* Generated {0} *)".format(time.strftime("%c"))

def gen_baseline(prog, path, base):
    test          = base + TEST_EXT
    test_path     = join(path, test)
    baseline_path = join(path, base + BASELINE_EXT)
    print("Generating baseline for {0}...".format(test_path))
    try:
        output_string = run_test(prog, path, base)
    except subprocess.CalledProcessError as err:
        output_string = "An error was raised by this test: {0}\n{1}".format(err.returncode, err.output)
        print("Encountered an error running test {0}".format(test_path))
    with open(baseline_path, 'w') as f:
        f.write(gen_timestamp() + '\n')
        f.write(output_string)

def gen_baselines(prog, root):
    for path, base in find_tests(root):
        gen_baseline(prog, path, base)

def print_usage(args):
    print("Usage: {0} <prog> <test|testdir>".format(args[0]))

def main(args):
    if len(args) == 3:
        prog = args[1]
        path = args[2]
        if not os.path.exists(prog):
            print_usage(args)
        elif os.path.exists(path) and os.path.isdir(path):
            gen_baselines(prog, path)
        else:
            path, filename = os.path.split(path)
            base, ext = splitext(filename)
            if ext != TEST_EXT:
                print_usage(args)
            else:
                gen_baseline(prog, path, base)
    else:
        print_usage(args)

if __name__ == '__main__':
    main(sys.argv)
