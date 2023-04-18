import matplotlib.pyplot as plt
import numpy as np
from sys import argv, exit

if len(argv) < 3:
  print("Usage: python", argv[0], "num_processors", "[TEST.out]...")
  exit(1)

num_processors = int(argv[1])
fig, ax = plt.subplots()

x = [i for i in range(1, num_processors + 1)]

y_max = -1

for f_str in argv[2:]:
  with open(f_str) as f:
    times = []
    for line in f:
      times.append(float(line))

    if len(times) != num_processors + 1:
      raise ValueError("%s has wrong number of lines: Expected %d, Actual %d"
                       % (f_str, num_processors + 1, len(times)))
    
    sequential_time = times[0]
    y = [sequential_time / time for time in times[1:]]
    y_max = max(np.max(y), y_max)
    ax.plot(x, y, label=f_str.split('/')[-1].split('.')[0])
y_lin = x
ax.plot(x, y_lin, '-.')

plt.xlabel("Number of Processors")
plt.ylabel("Speed Up over Sequential Implementation")
if len(argv) > 3:
  plt.legend()
  plt.ylim(0, 1.5 * y_max)
else:
  plt.ylim(0, 20)
plt.show()
