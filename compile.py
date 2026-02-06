import subprocess
import sys


if __name__ == '__main__':
    src_name = sys.argv[1]
    output_name = '.'.join(src_name.split('.')[:-1]) # is this a hack?
    preprocessed = output_name + '.i'
    assembly = output_name + '.s'
    subprocess.run(['gcc', '-E', '-P', src_name, '-o', preprocessed])
    print(f'Would compile {preprocessed} to {assembly} here...')
    if '-S' in sys.argv:
        sys.exit(0)
    subprocess.run(['gcc', assembly, '-o', output_name])
