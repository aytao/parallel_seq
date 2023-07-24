import os
from sys import argv, exit, stderr


def main():
  if len(argv) != 3:
    print("Usage: python", argv[0], "[time name]", "[max num_domains]")
    exit(1)

  time_name = argv[1]
  max_num_domains = int(argv[2])

  command_fmt = "dune exec -- %s -num_domains %d"
  for i in range(1, max_num_domains + 1):
    print("Running '%s' with %d domain(s)..." %
          (time_name, i), file=stderr, end="")
    stderr.flush()

    os.system(command_fmt % (time_name, i))

    print("Done", file=stderr)
    stderr.flush()


if __name__ == "__main__":
  main()
