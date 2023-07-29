import os
from sys import argv, exit, stderr, stdout


def main():
  if len(argv) < 5:
    print("Usage: python", argv[0], "[file_path_prefix]", "[max num_domains]", "[repeats]", "[index_file]", "[--dry_run]")
    exit(1)

  file_path_prefix = argv[1]
  max_domains = int(argv[2])
  num_repeats = int(argv[3])
  index_file = argv[4]
  if len(argv) == 6:
    dry_run = True
  else:
    dry_run = False

  all_tests = [
    ("inverted_index", "-i " + index_file),
    ("block_matrix_mul", "-n 2000")
  ]

  def get_command(time_test, args):
    filename = file_path_prefix + time_test
    command = ["python",
               "scripts/run_speedup_test.py",
               "\"%s\"" % time_test,
               "%d" % max_domains,
               "%d" % num_repeats,
               "\"%s\"" % args]
    if dry_run:
      command.append("--dry-run")
    command.append("> %s" % filename)
    
    return " ".join(command)
  
  for test in all_tests:
    time_test, args = test
    command = get_command(time_test, args)

    print("RUNNING '%s'\n\n" % time_test, file=stderr, end="")
    print(command, file=stderr)  
    stderr.flush()
    os.system(command)

    print("DONE WITH '%s'\n\n" % time_test, file=stderr, end="")
    stderr.flush()


if __name__ == "__main__":
  main()
