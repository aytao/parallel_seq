import matplotlib.pyplot as plt
import numpy as np
import sys

times = []

for line in sys.stdin:
  times.append(float(line))

sequential_time = times[0]

y = [sequential_time / time for time in times[1:]]

x = [i for i in range(1, len(y) + 1)]

fig, ax = plt.subplots()
ax.plot(x, y)
plt.show()