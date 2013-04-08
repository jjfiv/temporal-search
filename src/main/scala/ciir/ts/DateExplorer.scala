package ciir.ts

// rename galago, avoid RSI
import org.lemurproject.galago.{core => GalagoCore}
import org.lemurproject.galago.{tupleflow => GalagoTupleflow}

import GalagoCore.retrieval.LocalRetrieval
import GalagoTupleflow.Parameters

import gnu.trove.map.hash._

class LocalDateInfo(val retrieval: LocalRetrieval) {
  var cache = new TIntIntHashMap
  
  private def retrieveDate(documentName: String): Int = {
    var parms = new Parameters
    parms.set("terms", false)
    parms.set("tags", false)
    parms.set("text", false)
    parms.set("metadata", true)
    
    var dateStr = retrieval.getDocument(documentName, parms).metadata.get("date")
    
    if(dateStr == null) { return -1 }
    
    dateStr = dateStr.filter(_.isDigit)
    try {
      return dateStr.toInt
    } catch {
      case nfe: NumberFormatException => return -1
    }
  }
  
  private def retrieveDate(doc: Int): Int = try {
    retrieveDate(retrieval.getDocumentName(doc))
  } catch {
    case e: java.io.IOException => -1
  }
  
  def getDate(doc: Int) = {
    if(cache.containsKey(doc)) {
      cache.get(doc)
    } else {
      val date = retrieveDate(doc)
      cache.put(doc, date)
      date
    }
  }
}

object DateCurve {
  def ofTroveHash(data: TIntIntHashMap) = {
    val dates = data.keys
    val minDate = dates.min
    val maxDate = dates.max
    val numDates = (maxDate - minDate) + 1

    var counts = new Array[Int](numDates)

    dates.foreach(date => {
      val count = data.get(date)
      if(count > 0 && date >= minDate && date <= maxDate) {
        val index = date - minDate
        counts(index) += count
      }
    })

    DateCurve(minDate, counts)
  }
}

case class DateCurve(val startDate: Int, val counts: Array[Int]) {
  def numDates = counts.size
  def endDate = startDate + numDates
}

object DateExplorer {
  var UsefulCountMinimum = 50

  def buildIndex(args: Array[String]) {
    println("DateExplorer::buildIndex ["+args.mkString(",")+"]")

    // find any non-file arguments
    args.find(fn => !Util.fileExists(fn)) match {
      case Some(fileName) => {
        Console.err.println("Tried to build an index from non-existent file \""+fileName+"\"")
        sys.exit(-1)
      }
      case None => { }
    }
    
    var curvesKept = 0
    var curvesTouched = 0

    // handle each argument as a galago index
    args.map(indexPath => {
      val retrieval = GalagoIndexUtil.retrievalFromPath(indexPath)
      val index = retrieval.getIndex
      
      var dateInfo = new LocalDateInfo(retrieval)

      Util.timed("inspect postings", { 
      GalagoIndexUtil.forKeyInIndex(index, "postings", (term,docIter) => {
        // only touch ascii terms for now, they're more likely OCR issues than valid identifiers
        // TODO
        if(term.forall(_.isLetterOrDigit)) {
          // investigate current term
          var hits = new TIntIntHashMap
          var hitCount = 0
        
          curvesTouched += 1

          if(curvesTouched % 10000 == 0) { println( "# curvesTouched = "+curvesTouched) }

          GalagoIndexUtil.forDocInInvertedList(docIter, (doc, count) => {
            val date = dateInfo.getDate(doc)
            if(date > 0) {
              hits.adjustOrPutValue(date, count, count)
              hitCount += count
            }
          })
          
          if(hitCount >= UsefulCountMinimum) {
              curvesKept += 1
          }
        }
      })
      })
    })

    println("Parsed "+curvesTouched +" and kept " + curvesKept)
  }

}
