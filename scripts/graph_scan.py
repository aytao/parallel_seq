import matplotlib.pyplot as plt
import matplotlib.ticker as ticker
import numpy as np
from sys import argv, exit

if len(argv) != 5:
  print("Usage: python", argv[0], "[Graph Title]" "[num_processors]", "[scan out]", "[array_copy out]")
  exit(1)

num_processors = int(argv[2])
fig, ax = plt.subplots()

x = [i for i in range(1, num_processors + 1)]

y_max = -1

with open(argv[3]) as f:
  repeats = int(f.readline())
  all_times = [float(line) for line in f]

  if len(all_times) != num_processors * repeats:
    raise ValueError("%s has wrong number of data lines: Expected %d, Actual %d"
                      % (argv[3], num_processors * repeats, len(all_times)))
  chunks = np.array_split(all_times, num_processors)
  times = np.mean(chunks, axis=1)
  y_max = max(np.max(times), y_max)
  ax.plot(x, times, label="parallel_scan")

with open(argv[4]) as f:
  f.readline()
  all_times = [float(line) for line in f]
  time = np.mean(all_times)
  times = [time for _ in range(num_processors)]
  ax.plot(x, times, label="Array.copy")

ax.xaxis.set_major_locator(ticker.MultipleLocator(20))
ax.yaxis.set_major_locator(ticker.MultipleLocator(5.0))
ax.tick_params(axis='both', which='major', labelsize=15)

plt.title(argv[1], loc = 'center', fontsize=20)
plt.xlabel("Number of Domains", fontsize = 15)
plt.ylabel("Speed Up", fontsize = 15)
if len(argv) > 4:
  plt.legend()
  plt.ylim(0, 1.5 * y_max)
else:
  plt.ylim(0, 20)
plt.show()
