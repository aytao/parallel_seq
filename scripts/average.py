import numpy as np
from sys import argv

with open(argv[1]) as f:
  f.readline()
  all_times = [float(line) for line in f]
  times = np.mean(all_times)
  print(times )
