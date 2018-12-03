from peg import PEG
import sys
import os

if len(sys.argv) < 3:
    print("USAGE: matcher <grammar file> <text file>")
    exit(1)

peg_file_name = sys.argv[1] 
text_file_name = sys.argv[2]

assert os.path.isfile(peg_file_name)
assert os.path.isfile(text_file_name)

peg = PEG.from_file(peg_file_name)
with open(text_file_name, 'r') as f:
    text = f.read()

parse_tree = peg.parse(text)
if parse_tree is None:
    print(f'Text "{text}" is not match this grammar')
else:
    print(f'Text "{text}" match. Parse tree:')
    parse_tree.draw()