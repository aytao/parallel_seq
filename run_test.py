import os, sys
from sys import argv, exit

def main():
  if len(argv) < 3:
    print("Usage: python", argv[0], "[time name]", "[max num_domains]")
    exit(1)

  command_fmt = "dune exec --no-build -- %s"

  command_fmt += (" -s=" + argv[3]) if len(argv) == 4 else ""

  test_name = argv[1]
  max_num_domains = int(argv[2])

  os.system("dune build")
  
  affinity_mask = {0}
  for i in range(1, max_num_domains + 1):
    os.sched_setaffinity(0, affinity_mask)
    command = command_fmt % test_name 
    
    print("Running '%s' ..." % command, file = sys.stderr, end = "")
    sys.stderr.flush()

    os.system(command)

    print("Done", file = sys.stderr)
    sys.stderr.flush()
    affinity_mask.add(i)

if __name__ == "__main__":
  main()
