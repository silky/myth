#!/usr/bin/python

from __future__ import print_function

import difflib
import os
from os.path import splitext, join
import subprocess
import sys

TEST_EXT = '.ml'
BASELINE_EXT = '.out'
FLAGS = ['-nosugar', '-noincomplete-warning']

def find_tests(root):
    tests = []
    for path, dirs, files in os.walk(root):
        files = [(f[0], f[1]) for f in [splitext(f) for f in files]]
        tests.extend([(path, f[0]) for f in files if f[1] == TEST_EXT])
    return tests

def run_test(prog, path):
    output_string = subprocess.check_output([prog] + FLAGS + [path], stderr=subprocess.STDOUT)
    return output_string.decode('utf-8').splitlines(1)

def extract_baseline_output(path):
    with open(path) as f:
        lines = f.readlines()
        # Strip the timestamp at the top of the output file
        lines = lines[1:]
        return lines

def create_diff(output, baseline, test_path, baseline_path):
    diff_gen = difflib.unified_diff(output, baseline, fromfile=test_path, tofile=baseline_path)
    diff = []
    for line in diff_gen:
        diff.append(line)
    return diff

def check_test(prog, path, base):
    test_path     = join(path, base + TEST_EXT)
    baseline_path = join(path, base + BASELINE_EXT)
    print("Checking {0}...".format(test_path))
    if not os.path.isfile(baseline_path):
        print("Baseline file not found: {0}".format(baseline_path))
        return
    try:
        output = run_test(prog, test_path)
    except subprocess.CalledProcessError as err:
        print("Encountered an error running test {0}".format(test_path))
        return
    baseline = extract_baseline_output(baseline_path)
    diff = create_diff(output, baseline, test_path, baseline_path)
    if (len(diff) > 0):
        print(''.join(diff))

def check_tests(prog, root):
    for path, base in find_tests(root):
        check_test(prog, path, base)

def print_usage(args):
    print("Usage: {0} <prog> <test|testdir>".format(args[0]))

def main(args):
    if len(args) == 3:
        prog = args[1]
        path = args[2]
        if not os.path.exists(prog):
            print_usage(args)
        elif os.path.exists(path) and os.path.isdir(path):
            check_tests(prog, path)
        else:
            path, filename = os.path.split(path)
            base, ext = splitext(filename)
            if ext != TEST_EXT:
                print_usage(args)
            else:
                check_test(prog, path, base)
    else:
        print_usage(args)

if __name__ == '__main__':
    main(sys.argv)
