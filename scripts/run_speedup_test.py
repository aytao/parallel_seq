import os
from sys import argv, exit, stderr, stdout

def main():
  if len(argv) < 4:
    print("Usage: python", argv[0], "[time name]", "[max num_domains]", "[repeats]",
          "[time name args]", "[--dry-run]")
    exit(1)
  
  def execute(command):
    if len(argv) == 6:
      print("")
      print(command)
    else:
      os.system(command)

  test_name = argv[1]
  max_num_domains = int(argv[2])
  repeats = int(argv[3])

  command = "dune exec -- time/commander/time.exe %s %s" % (test_name, argv[4])
  print(repeats)
  stdout.flush()

  print("Running '%s' sequentially..." % test_name, file=stderr, end="")
  stderr.flush()
  
  for _ in range(repeats):
    execute(command + " -f")

  print("Done", file=stderr)
  stderr.flush()
  
  command_fmt = command + " -num_domains %d"
  for i in range(1, max_num_domains + 1):
    print("Running '%s' with %d domain(s)..." %
          (test_name, i), file=stderr, end="")
    stderr.flush()
    for _ in range(repeats):
      execute(command_fmt % i)

    print("Done", file=stderr)
    stderr.flush()



if __name__ == "__main__":
  main()
