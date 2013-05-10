#!/usr/bin/python2

import sys
import csv
import numpy as np
import matplotlib.pyplot as plt

# using floats to stay generic
def curves_in_data(csv_rows):
  for row in csv_rows:
    yield row[0], [float(x) for x in row[1:]]

if __name__ == '__main__':
  rows = []
  if not sys.stdin.isatty():
    reader = csv.reader(sys.stdin)
    for row in reader:
      rows += [row]
  else:
    with open(sys.argv[1]) as f:
      reader = csv.reader(f)
      for row in reader:
        rows += [row]

  print "Given input: "
  #print " ".join(rows[0])

  Xdata = None
  Xlabel = None
  curves = []
  dim = -1

  for term, data in curves_in_data(rows):
    if dim == -1:
      dim = len(data)
      Xlabel = term
      Xdata = np.array(data)
    else:
      if len(data) != dim:
        print "Error, mismatched data lengths!"
        sys.exit(-1)
      curves += [(term, np.array(data))]

  if not Xlabel:
    print "Error, expected some data!"
    sys.exit(-1)

  fig = plt.figure()
  ax = fig.add_subplot(111)
  ax.set_xlabel("Year")
  ax.set_ylabel("Occurrences")
  
  for name, curve in curves:
    ax.plot(Xdata, curve, label=name)

  ax.legend()

  plt.show()


