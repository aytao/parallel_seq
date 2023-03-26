import os, sys
from sys import argv, exit

def main():
  if len(argv) < 3:
    print("Usage: python", argv[0], "[time name]", "[max num_domains]")
    exit(1)

  command_fmt = "dune exec --no-build -- %s -num_domains=%d"

  command_fmt += (" -s=" + argv[3]) if len(argv) == 4 else ""

  test_name = argv[1]
  max_num_domains = int(argv[2])

  os.system("dune build")
  
  for i in range(1, max_num_domains + 1):
    command = command_fmt % (test_name, i)
    os.system(command)

if __name__ == "__main__":
  main()