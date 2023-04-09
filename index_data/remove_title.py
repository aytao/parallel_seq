import os
import random
import sys
from sys import argv, exit


def main():
  if len(argv) < 3:
    print("Usage: python", argv[0], "[input file]", "[output file]")
    exit(1)

  input_file = argv[1]
  output_file = argv[2]

  contents = []
  with open(input_file, "r") as f:
    for line in (x.strip() for x in f):
      contents.append(line.split("@")[-1])

  with open(output_file, "w") as f:
    for i, line in enumerate(contents):
      f.write("%d@%s\n" % (i, line))


if __name__ == "__main__":
  main()
