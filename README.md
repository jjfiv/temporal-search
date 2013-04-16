temporal-search
===============

usage
-----

    scripts/run.py task-name [task-specific arguments]

where task-name is one of the available:

 - `barrel-gen` builds barrels for duplication detection
 - `term-plot-dates` plot dates runs a set of queries against a Galago index and creates a csv containing the date information
 - `dates-build` takes a [Galago][galago] Index and caches date curves for each term
 - `dates-kmeans` takes a file made with dates-build and clusters all the terms present based on cosine similarity
 - `inspect` prints some basic information about a [Galago][galago] Index
 - `count-books`, `count-books-graph`, `count-books-stat` a task run on [swarm](http://cs.umass.edu/~swarm) to collect statistics about the books available as part of the [Million Books project](http://books.cs.umass.edu)

[galago]: http://www.lemurproject.org/galago.php

