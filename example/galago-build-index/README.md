Galago Index Building Example
===

This is a configuration file and book list that works with a set of mbtei books and Galago 3.4 (hg id = c714e5019eb9+).

Getting Galago
---

    hg clone http://hg.code.sf.net/p/lemur/galago lemur-galago -r c714e5019eb9 
    galago_root=`pwd`/lemur-galago
    cd $galago_root
    mvn -DskipTests install

Here, we clone the known decent revision of galago, save our galago location in a variable, change directory into it, and instruct maven to install it locally while skipping the tests. (They are time-consuming and may fail for reasons related to network connection and OS).

Modifications to Galago
---
There is one source change to Galago necessary. At the above listed revision, the class MBTEIBookParser must be made public so it can be loaded through reflection as an externalParser.

    edit ${galago_root}/contrib/src/main/java/ciir/proteus/galago/parse/MBTEIBookParser.java
    mvn -DskipTests install # recompile & reinstall galago

Modifications to input.list
---
The books specified in ``input.list`` are specified with absolute paths on my machine. You'll have to acquire some mbtei documents from swarm and adjust this file as appropriate.

Building the index
---

Then, since we need to have the contrib sources on our classpath while building the index, the command to build the index (from the root of this git repo) is:

    ${galago_root}/contrib/target/appassembler/bin/galago build example/galago-build-index/conf.json


Checking the result
---

Using our friendly launcher tool ``scripts/run.py``, we can do things to this new index.

    ./scripts/run.py inspect example/galago-build-index/output


Running again
---

With the flag ``deleteJobDir=false``, you will have to manually remove the ``galagoJobDir`` after running, but this allows better diagnosing of errors.

