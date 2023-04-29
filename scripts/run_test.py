import os
import sys
from sys import argv, exit


def main():
  if len(argv) < 3:
    print("Usage: python", argv[0], "[time name]", "[max num_domains]")
    exit(1)

  test_name = argv[1]
  max_num_domains = int(argv[2])

  command = "dune exec -- %s" % test_name

  print("Running '%s' sequentially..." % test_name, file=sys.stderr, end="")
  sys.stderr.flush()

  os.system(command + " -f")

  print("Done", file=sys.stderr)
  sys.stderr.flush()

  affinity_mask = {0}
  for i in range(1, max_num_domains + 1):
    os.sched_setaffinity(0, affinity_mask)

    print("Running '%s' with %d domain(s)..." %
          (test_name, i), file=sys.stderr, end="")
    sys.stderr.flush()

    os.system(command)

    print("Done", file=sys.stderr)
    sys.stderr.flush()
    affinity_mask.add(i)


if __name__ == "__main__":
  main()
