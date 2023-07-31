import matplotlib.pyplot as plt
import matplotlib.ticker as ticker
import numpy as np
from sys import argv, exit

if len(argv) < 4:
  print("Usage: python", argv[0], "[Graph Title]" "[num_processors]", "[test out]", "[--reverse]")
  exit(1)

num_processors = int(argv[2])
fig, ax = plt.subplots()

x = [i for i in range(1, num_processors + 1)]

y_max = -1

with open(argv[3]) as f:
  repeats = int(f.readline())
  all_times = [float(line) for line in f]

  if len(all_times) != (num_processors + 1) * repeats:
    raise ValueError("%s has wrong number of data lines: Expected %d, Actual %d"
                      % (argv[3], (num_processors + 1) * repeats, len(all_times)))
  chunks = np.array_split(all_times, num_processors + 1)
  times = np.mean(chunks, axis=1)
  sequential_time = times[0]
  y = [sequential_time / time for time in times[1:]]
  if len(argv) == 5:
    y.reverse()

  for i, time in enumerate(y):
    print(i + 1, sequential_time / time)

  def capitalize(string):
    string.capitalize()

  name = argv[3].split("/")[-1]
  parts = name.split("_")
  map(capitalize, parts)
  label = " ".join(parts)

  y_max = max(np.max(y), y_max)
  ax.plot(x, y, label=label)

y_lin = x
ax.plot(x, y_lin, '-.', label="y=x")

ax.xaxis.set_major_locator(ticker.MultipleLocator(20))
# ax.yaxis.set_major_locator(ticker.MultipleLocator(5.0))
ax.tick_params(axis='both', which='major', labelsize=15)

plt.title(argv[1], loc = 'center', fontsize=20)
plt.xlabel("Number of domains", fontsize = 15)
plt.ylabel("Execution time (sec.)", fontsize = 15)

plt.legend()
plt.ylim(0, 2 * y_max)
plt.show()
