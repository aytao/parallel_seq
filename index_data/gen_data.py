import os
import random
import sys
from sys import argv, exit


def main():
  if len(argv) < 4:
    print("Usage: python", argv[0], "[input file]",
          "[num output lines]", "[output file]")
    exit(1)

  input_file = argv[1]
  num_output_lines = int(argv[2])
  output_file = argv[3]

  contents = []
  with open(input_file, "r") as f:
    for line in (x.strip() for x in f):
      contents.append(line.split("@")[-1])

  with open(output_file, "w") as f:
    for i in range(0, num_output_lines):
      content = contents[random.randrange(0, len(contents))]
      f.write("%d@%s\n" % (i, content))


if __name__ == "__main__":
  main()
