import matplotlib.pyplot as plt
import numpy as np
import sys

y = []

for line in sys.stdin:
  y.append(1 / float(line))

x = [i for i in range(1, len(y) + 1)]

fig, ax = plt.subplots()
ax.plot(x, y)
plt.show()