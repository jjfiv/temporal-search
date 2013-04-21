package ciir.ts.index
import ciir.ts.Util

import org.lemurproject.galago.{core => GalagoCore}
import org.lemurproject.galago.{tupleflow => GalagoTupleflow}

import GalagoCore.index.disk.{DiskIndex, PositionIndexWriter}
import GalagoCore.index.IndexPartReader
import GalagoTupleflow.{Parameters, FakeParameters}

object GalagoIter {
  import GalagoCore.retrieval.iterator._
  import GalagoCore.index.Index
  import GalagoCore.index.{KeyIterator, ValueIterator}
  import GalagoCore.retrieval.processing.ScoringContext
  
  def keys(keyIter: KeyIterator)(op: =>Unit) {
    keyIter.reset()
    while(!keyIter.isDone) {
      op
      keyIter.nextKey
    }
  }
  
  def stringKeys(keyIter: KeyIterator) = new Iterator[String] {
    keyIter.reset()
    def hasNext = !keyIter.isDone
    def next() = {
      val str = keyIter.getKeyString
      keyIter.nextKey
      str
    }
  }

  def byteKeys(keyIter: KeyIterator) = new Iterator[Array[Byte]] {
    keyIter.reset()
    def hasNext = !keyIter.isDone
    def next() = {
      val bytes = keyIter.getKey
      keyIter.nextKey
      bytes
    }
  }

  def docs(valueIter: ValueIterator) = new Iterator[Int] {
    var iter = valueIter.asInstanceOf[MovableIterator]
    iter.reset()
    var context = new ScoringContext
    def hasNext = !iter.isDone
    def next() = {
      val doc = iter.currentCandidate
      context.document = doc
      iter.movePast(doc)
      doc
    }
  }

  def begins(valueIter: ValueIterator) = new Iterator[Int] {
    var extents = valueIter.asInstanceOf[MovableExtentIterator].extents()
    var i=0
    def hasNext = i < extents.size
    def next() = {
      val x = extents.begin(i)
      i+=1
      x
    }
  }

  type PostingsIterator = MovableCountIterator with ValueIterator with MovableExtentIterator
}

object DateFilter {
  def quit(msg: String) {
    Util.quit("*** error: "+msg+"\n" +
      "usage: input-postings filtered-postings min-count")
  }
  def main(args: Array[String]) {
    if(args.size != 3 || args.size != 2) {
      quit("bad number of arguments")
    }

    val inputPath = args(0)
    val outputPath = args(1)
    val minimumTF = {
      if(args.size == 2) {
        1
      } else {
        args(2).toInt
      }
    }

    var inputReader = DiskIndex.openIndexComponent(inputPath).asInstanceOf[IndexPartReader]
    var outputWriter = {
      var parms = new Parameters
      parms.set("filename", outputPath)
      new PositionIndexWriter(new FakeParameters(parms))
    }

    var keyIter = inputReader.getIterator
    GalagoIter.keys(keyIter) {
      var postingsIter = keyIter.getValueIterator.asInstanceOf[GalagoIter.PostingsIterator]
      
      // make sure we satisfy the minimum term count
      var termCount = 0
      // don't bother checking if this is a merge-job
      if(minimumTF > 1) {
        GalagoIter.docs(postingsIter).takeWhile(doc => {
          termCount += postingsIter.count()
          termCount < minimumTF
        })
      }

      if(termCount >= minimumTF) {
        outputWriter.processWord(keyIter.getKey)
        GalagoIter.docs(postingsIter).foreach(doc => {
          outputWriter.processDocument(doc)
          GalagoIter.begins(postingsIter).foreach(pos => {
            outputWriter.processPosition(pos)
          })
        })
      }

    }
    
    println("Done")
  }
}

