import os
from sys import argv, exit, stderr, stdout


def main():
  if len(argv) != 4:
    print("Usage: python", argv[0], "[time name]", "[max num_domains]", "[repeats]")
    exit(1)

  time_name = argv[1]
  max_num_domains = int(argv[2])
  repeats = int(argv[3])

  command_fmt = "dune exec -- %s -num_domains %d"
  print(repeats)
  stdout.flush()
  for i in range(1, max_num_domains + 1):
    print("Running '%s' with %d domain(s)..." %
          (time_name, i), file=stderr, end="")
    stderr.flush()
    for _ in range(repeats):
      os.system(command_fmt % (time_name, i))

    print("Done", file=stderr)
    stderr.flush()


if __name__ == "__main__":
  main()
