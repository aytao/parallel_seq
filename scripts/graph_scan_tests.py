import matplotlib.pyplot as plt
import matplotlib.ticker as ticker
import numpy as np
from sys import argv, exit

if len(argv) < 4:
  print("Usage: python", argv[0], "[Graph Title]" "[num_processors]", "[scan out]...")
  exit(1)

num_processors = int(argv[2])
fig, ax = plt.subplots()

x = [i for i in range(1, num_processors + 1)]

y_max = -1

for f_str in argv[3:]:
  with open(f_str) as f:
    repeats = int(f.readline())
    all_times = [float(line) for line in f]

    if len(all_times) != num_processors * repeats:
      raise ValueError("%s has wrong number of data lines: Expected %d, Actual %d"
                        % (f_str, num_processors * repeats, len(all_times)))
    chunks = np.array_split(all_times, num_processors)
    times = np.mean(chunks, axis=1)
    y_max = max(np.max(times), y_max)
    ax.plot(x, times, label=f_str.split("/")[-1])

# ax.xaxis.set_major_locator(ticker.MultipleLocator(20))
ax.yaxis.set_major_locator(ticker.MultipleLocator(5.0))
ax.tick_params(axis='both', which='major', labelsize=15)

plt.title(argv[1], loc = 'center', fontsize=20)
plt.xlabel("Number of domains", fontsize = 15)
plt.ylabel("Execution time (sec.)", fontsize = 15)
if len(argv) > 4:
  plt.legend()
  plt.ylim(0, 1.1 * y_max)
else:
  plt.ylim(0, 20)
plt.show()
