#!/usr/bin/env python3
import os
import subprocess
import sys


if __name__ == '__main__':
    src_name = sys.argv[1]
    output_name = '.'.join(src_name.split('.')[:-1]) # is this a hack?
    preprocessed = output_name + '.i'
    assembly = output_name + '.S'
    subprocess.run(['gcc', '-E', '-P', src_name, '-o', preprocessed])

    proc = subprocess.run([os.path.abspath('/Users/pfogg/Documents/src/writing-a-compiler/target/debug/writing-a-compiler'), preprocessed])

    if proc.returncode != 0:
        sys.exit(proc.returncode)

    if '-S' in sys.argv:
        sys.exit(0)
    subprocess.run(['gcc', assembly, '-o', output_name])
