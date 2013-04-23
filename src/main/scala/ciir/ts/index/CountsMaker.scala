package ciir.ts.index
import ciir.ts._

import org.lemurproject.galago.{core => GalagoCore}
import org.lemurproject.galago.{tupleflow => Tupleflow}

import GalagoCore.index.disk.{DiskIndex, CountIndexWriter}
import GalagoCore.index.IndexPartReader
import Tupleflow.{Parameters, FakeParameters}

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
    Galago.keys(keyIter) {

      if(currentKey % 10000 == 0){
        println("Key: "+currentKey+"/"+totalKeys)
      }
      
      // make sure we satisfy the minimum term count
      var termCount = 0
      var cIter = keyIter.getValueIterator.asInstanceOf[Galago.CountsIter]
      Galago.docs(cIter)(doc => {
        val count = cIter.count()
        termCount += count
      })

      if(termCount >= minimumTF) {
        var pIter = keyIter.getValueIterator.asInstanceOf[Galago.CountsIter]
        outputWriter.processWord(keyIter.getKey)
        Galago.docs(pIter)(doc => {
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

  def makeDateCounts(args: Array[String]) {
    if(args.size != 1) {
      Util.quit("*** error: expected argument: index-path")
    }

    val dates = new DateRetrieval(args(0))
    val indexPath = args(0)

    if(dates.index.hasPart("date-counts"))
      Util.quit("given index already has `date-counts`")
    
    var inputReader = dates.index.getPart("just-counts")

    var outputWriter = {
      var parms = new Parameters
      parms.set("filename", args(0)+"/"+"date-counts")
      new CountIndexWriter(new FakeParameters(parms))
    }

    var totalKeys = inputReader.getManifest.get("statistics/vocabCount", 0L)
    var currentKey = 0
    
    println("parsing index with "+totalKeys)

    var keyIter = inputReader.getIterator
    Galago.keys(keyIter) {

      if(currentKey % 10000 == 0){
        println("Key: "+currentKey+"/"+totalKeys)
      }
      
      // sweep across documents, building a date vector
      var dateVector = new Array[Int](dates.NumYears)
      var cIter = keyIter.getValueIterator.asInstanceOf[Galago.CountsIter]
      Galago.docs(cIter)(doc => {
        val date = dates.dateInfo.getDate(doc)
        if(dates.validDate(date)) {
          val idx = date - dates.StartYear
          dateVector(idx) += cIter.count()
        }
      })

      if(dateVector.sum > 0) {
        // write the date vector to the output file
        outputWriter.processWord(keyIter.getKey)
        dateVector.zipWithIndex.foreach {
          case (0, _) => { }
          case (count, idx) => {
            val date = idx
            outputWriter.processDocument(date)
            outputWriter.processTuple(count)
          }
        }
      }

      currentKey += 1
    }
    outputWriter.close()
    println("Done")
    
  }
}

