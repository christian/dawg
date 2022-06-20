An implementation of the DAWG data structure as described here http://stevehanov.ca/blog/?id=115

# Running

This project uses sbt so to run it:

    $ sbt
    $ sbt:dawg > run

The project produces a dot file in Graphviz format which portraits the DAWG stored in memory. To view the graph as a png, one can use:

    $ dot -Tpng dawg.dot > output.png


