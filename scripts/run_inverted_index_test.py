import os
from sys import argv, exit, stderr, stdout

def main():
  if len(argv) < 4:
    print("Usage: python", argv[0], "[min num_domains]", "[max num_domains]", "[repeats]",
          "[test_file]", "[--dry-run]")
    exit(1)
  
  def execute(command):
    if len(argv) == 6:
      print("")
      print(command)
    else:
      os.system(command)

  test_name = "inverted_index"
  min_num_domains = int(argv[1])
  max_num_domains = int(argv[2])
  repeats = int(argv[3])

  filename = "/tmp/%s" % argv[4].split("/")[-1]
  execute("cp -f %s %s" % (argv[4], filename))
  execute("dune build")
  execute("cp -f _build/default/time/commander/time.exe /tmp/time_test.exe")

  command = "/tmp/time_test.exe %s -i %s" % (test_name, filename)
  print(repeats)
  stdout.flush()

  print("Running '%s' sequentially..." % test_name, file=stderr, end="")
  stderr.flush()
  
  for _ in range(repeats):
    execute("dd of=%s oflag=nocache conv=notrunc,fdatasync count=0" % filename)
    execute(command + " -f")

  print("Done", file=stderr)
  stderr.flush()
  
  command_fmt = command + " -num_domains %d"
  for i in range(min_num_domains, max_num_domains + 1):
    print("Running '%s' with %d domain(s)..." %
          (test_name, i), file=stderr, end="")
    stderr.flush()
    for _ in range(repeats):
      execute("dd of=%s oflag=nocache conv=notrunc,fdatasync count=0 2> /dev/null" % filename)
      execute(command_fmt % i)

    print("Done", file=stderr)
    stderr.flush()



if __name__ == "__main__":
  main()
