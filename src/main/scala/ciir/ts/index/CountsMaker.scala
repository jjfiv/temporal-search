package ciir.ts.index

import org.lemurproject.galago.{core => GalagoCore}
import org.lemurproject.galago.{tupleflow => GalagoTupleflow}

import GalagoCore.index.disk.{DiskIndex, CountIndexWriter}
import GalagoCore.index.IndexPartReader
import GalagoTupleflow.{Parameters, FakeParameters}

object CountsMaker {
  def quit(msg: String) {
    ciir.ts.Util.quit("*** error: "+msg+"\n" +
      "usage: input-postings filtered-counts min-count")
  }
  def main(args: Array[String]) {
    if(args.size != 3) {
      quit("bad number of arguments")
    }
    
    val inputPath = args(0)
    val outputPath = args(1)
    val minimumTF = args(2).toInt
    if(minimumTF < 1) { // otherwise this is dumb
      quit("bad value for min-count")
    }

    var inputReader = DiskIndex.openIndexComponent(inputPath).asInstanceOf[IndexPartReader]
    
    var outputWriter = {
      var parms = new Parameters
      parms.set("filename", outputPath)
      new CountIndexWriter(new FakeParameters(parms))
    }

    var keptKeys = 0
    var totalKeys = inputReader.getManifest.get("statistics/vocabCount", 0L)
    var currentKey = 0
    
    println("parsing index with "+totalKeys)

    var keyIter = inputReader.getIterator
    GalagoIter.keys(keyIter) {

      if(currentKey % 10000 == 0){
        println("Key: "+currentKey+"/"+totalKeys)
      }
      
      // make sure we satisfy the minimum term count
      var termCount = 0
      var cIter = keyIter.getValueIterator.asInstanceOf[GalagoIter.Counts]
      GalagoIter.docs(cIter)(doc => {
        val count = cIter.count()
        assert(count != 0)
        termCount += count
        termCount < minimumTF
      })

      assert(termCount > 0)

      if(termCount >= minimumTF) {
        var pIter = keyIter.getValueIterator.asInstanceOf[GalagoIter.Counts]
        outputWriter.processWord(keyIter.getKey)
        GalagoIter.docs(pIter)(doc => {
          outputWriter.processDocument(doc)
          outputWriter.processTuple(termCount)
        })
        keptKeys += 1
      }
      
      currentKey += 1
    }
    println("\n")

    println("processed "+totalKeys+" keys, kept "+keptKeys)
    outputWriter.close()
    println("Done")
  }
}

