import argparse

parser = argparse.ArgumentParser(
    description="Perform some textual analysis on our directory of cipher texts.\n" +
                "Use with option -v to run with human intervention\n" +
                "Use with option -g to generate common stats on files, or don't use it to reuse old ones"
)

parser.add_argument("-v", action='store_true')
parser.add_argument("-g", action='store_true')
