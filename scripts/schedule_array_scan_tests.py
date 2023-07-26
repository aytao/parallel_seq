import os
from sys import argv, exit, stderr, stdout


def main():
  if len(argv) < 4:
    print("Usage: python", argv[0], "[max num_domains]", "[repeats]", "[time test]...")
    exit(1)

  file_path_prefix = argv[0]
  max_domains = int(argv[1])
  num_repeats = int(argv[2])

  def get_command(time_test):
    filename = file_path_prefix + time_test
    command = ["/u/aytao/.venv/bin/python",
               "scripts/run_array_scan_test.py",
               "\"%s\"" % time_test,
               "%d" % max_domains,
               "%d" % num_repeats,
               "> %s" % filename
               ]
    
    return " ".join(command)

  for time_test in argv[3:]:
    command = get_command(time_test)

    print("RUNNING '%s'\n\n" % time_test, file=stderr, end="")
    print(command, file=stderr)  
    stderr.flush()
    os.system(command)

    print("DONE WITH '%s'\n\n" % time_test, file=stderr, end="")
    stderr.flush()


if __name__ == "__main__":
  main()
