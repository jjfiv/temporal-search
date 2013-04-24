package ciir.ts

import gnu.trove.map.hash._
import collection.mutable.ArrayBuilder

class LocalDateInfo(val numDocs: Int, val retrieval: Galago.Retrieval) {
  var linearCache = Array.fill(numDocs) { -1 }
  
  private def retrieveDate(documentName: String): Int = {
    val parms = Galago.parameters(Map(
      "terms" -> "false",
      "tags" -> "false",
      "text" -> "false",
      "metadata" -> "true"))
    
    val doc = retrieval.getDocument(documentName, parms)
    val metadata = doc.metadata
    var dateStr = metadata.get("date")
    
    if(dateStr == null) { return -1 }
    
    dateStr = dateStr.filter(_.isDigit)
    try {
      val numericDate = dateStr.toInt
      if(numericDate < 0 || numericDate > 2013)
        return -1
      numericDate
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
    if(linearCache(doc) != -1)
      linearCache(doc)
    else {
      val date = retrieveDate(doc)
      linearCache(doc) = date
      date
    }
  }

  // reverse map; for building fake TF curves
  def getDocsForDate(date: Int) = {
    linearCache.indices.flatMap {
      case doc => {
        if(getDate(doc) == date)
          Some(doc)
        else
          None
      }
    }
  }
}

object CurveFaker {
  def stepFunction(dates: DateRetrieval, start: Int, weight: Int) = {
    dates.approxDateToTF(Array.tabulate(dates.NumYears) {
      case idx => {
        val date = idx + dates.StartYear

        if(date >= start) {
          weight
        } else 0
      }
    })
  }
}

case class DocDateScore(val docId: Int, val date: Int, val score: Int) { }
case class SimilarTerm(val key: String, val score: Double, val data: Array[Int]) extends Ordered[SimilarTerm] {
  def compare(that: SimilarTerm) = score.compare(that.score)
}

// hooray generics
// manifest is a trick to keep more type information
// and ordered implies that A will have a compare function defined
class RankedList[A <% Ordered[A]: Manifest](val numHits: Int) {
  private var results = new Array[A](numHits)
  private var count = 0

  def insert(newest: A) {
    if(count < numHits) {
      results(count) = newest
      count += 1
      return
    }

    // discard new things with tiny scores, special case
    if(results.last > newest) {
      return
    }

    // chop current array in pieces and reassemble
    val (better, worse) = results.partition(_ > newest)
    results = (better :+ newest) ++ worse.dropRight(1)
  }

  def done = {
    if (count <= numHits) {
      results.take(count).sorted.reverse
    } else {
      results.reverse
    }
  }
}


class BasicIndex(var index: Galago.Index) {
  val postings = index.getIndexPart("postings")
  //val counts = index.getIndexPart("just-counts")
  val (numDocs, maxTFVector) ={
    var size = 0
    var tfVector = new ArrayBuilder.ofInt
    Galago.lengths(index.getLengthsIterator)(len => {
      size += 1
      tfVector += len
    })
    (size, tfVector.result())
  }

  def hasPart(partName: String) = index.getIndexPart(partName) != null
  def getPart(partName: String) = {
    assert(hasPart(partName))
    index.getIndexPart(partName)
  }

  def getDocName(id: Int) = index.getName(id)
  def getLength(id: Int) = maxTFVector(id)

  def eachPosting(op: (String,Array[Int])=>Unit) {
    val bestIndexPart = {
      Set("just-counts", "counts", "postings", "postings.porter").map(index.getIndexPart).filter(_ != null).head
    }
    
    var keyIter = bestIndexPart.getIterator
    
    Util.timed("iterate over all counts:", {
      Galago.keys(keyIter) {
        var data = new Array[Int](numDocs)

        // read in posting list, accumulate counts
        var cIter = keyIter.getValueIterator.asInstanceOf[Galago.CountsIter]
        Galago.docs(cIter) { data(_) = cIter.count() }
        
        // handle this posting
        op(keyIter.getKeyString, data)
      }
    })
  }

  def eachDatePosting(op: (String,Array[Int])=>Unit) {
    var keyIter = getPart("date-counts").getIterator
    
    Util.timed("iterate over all date counts:", {
      Galago.keys(keyIter) {
        var data = new Array[Int](100)

        assert(!keyIter.isDone)
        // read in posting list, accumulate counts
        var cIter = keyIter.getValueIterator.asInstanceOf[Galago.CountsIter]
        Galago.docs(cIter) { data(_) = cIter.count() }
        
        // handle this posting
        op(keyIter.getKeyString, data)
      }
    })
  }

}

class DateRetrieval(indexDir: String) {
  val retrieval = Galago.openRetrieval(indexDir)
  val index = new BasicIndex(retrieval.getIndex)
  val dateInfo = new LocalDateInfo(index.numDocs, retrieval)

  val StartYear = 1820
  val NumYears = 100
  val EndYear = StartYear + NumYears - 1

  def validDate(d: Int) = d >= StartYear && d <= EndYear

  def numDocs = index.numDocs
  def getDocName(id: Int) = index.getDocName(id)

  def dateSearch(query: String): Array[DocDateScore] = {
    Galago.doCountsQuery(retrieval, numDocs, query).flatMap {
      case DocCount(doc, count) => {
        val date = dateInfo.getDate(doc)
        if(validDate(date)) {
          Some(DocDateScore(doc, date, count))
        } else None
      }
    }
  }
  def search(query: String): Array[Int] = {
    var tfVector = new Array[Int](numDocs)
    Galago.doCountsQuery(retrieval, numDocs, query).foreach {
      case DocCount(doc, count) => tfVector(doc) = count
    }
    tfVector
  }
  def toDateVector(tfVector: Array[Int]): Array[Int] = {
    assert(tfVector.size == numDocs)
    var dateTF = new Array[Int](NumYears)
    tfVector.zipWithIndex.foreach {
      case (count, doc) => {
        val date = dateInfo.getDate(doc)
        if(validDate(date)) {
          dateTF(date - StartYear) += count
        }
      }
    }
    dateTF
  }

  def approxDateToTF(dateVector: Array[Int]): Array[Int] = {
    assert(dateVector.size == NumYears)
    
    var tfVector = new Array[Int](numDocs)
    
    dateVector.zip(StartYear to EndYear).foreach {
      case (0, _) => { }
      case (weight, year) => {
        val docs = dateInfo.getDocsForDate(year)
        val meanWeight = (weight.toDouble / docs.size.toDouble).toInt
        docs.foreach { tfVector(_) = meanWeight }
      }
    }
    tfVector
  }

  def findSimilarTF(queryCurve: Array[Int], numResults: Int): Array[SimilarTerm] = {
    var results = new RankedList[SimilarTerm](numResults)
    index.eachPosting {
      case (term, curve) => {
        val similarityScore = Math.cosineSimilarity(curve, queryCurve)
        results.insert(SimilarTerm(term, similarityScore, curve.clone()))
      }
    }
    results.done
  }

  // group curves by quantSize year chunks and return the best matches for each time span
  // is this a plot of meaning over time? We'll find out
  def findSimilarQuantized(queryCurve: Array[Int], numResults: Int, quantSize: Int = 10) = {
    assert(queryCurve.size == NumYears)
    var numQuantiles = NumYears / quantSize
    var results = (0 until numQuantiles).map(q => {
      new RankedList[SimilarTerm](numResults)
    })
    
    index.eachDatePosting {
      case (term, curve) => {
        results.indices.foreach(idx => {
          val startIdx = idx*quantSize
          val endIdx = startIdx + quantSize - 1
          val subcurve = curve.slice(startIdx, endIdx)
          val qsubcurve = queryCurve.slice(startIdx, endIdx)

          // only look for matches if we have a nonzero curve in this quantile
          if(qsubcurve.sum != 0 && subcurve.sum != 0) {
            val score = Math.cosineSimilarity(subcurve, qsubcurve)
            // insert the result into the quantile's bucket
            results(idx).insert(SimilarTerm(term, score, subcurve.clone()))
          }
        })
      }
    }

    val finalResults = results.map(_.done)

    finalResults.zipWithIndex.foreach {
      case (res, idx) => {
        val quantileStart = StartYear + idx*quantSize
        val quantileEnd = quantileStart + quantSize - 1
        
        val startIdx = idx*quantSize
        val endIdx = startIdx + quantSize - 1
        val qsubcurve = queryCurve.slice(startIdx, endIdx)

        println("%d-%d - w=%d:".format(quantileStart, quantileEnd, qsubcurve.sum))
        res.foreach {
          case SimilarTerm(term, score, curve) => {
            println("  %s %.3f %d".format(term, score, curve.sum))
          }
        }
      }
    }
    
    finalResults
  }

  def findSimilarDate(queryCurve: Array[Int], numResults: Int): Array[SimilarTerm] = {
    assert(queryCurve.size == NumYears)
    var results = new RankedList[SimilarTerm](numResults)
    index.eachDatePosting {
      case (term, curve) => {
        val similarityScore = Math.cosineSimilarity(curve, queryCurve)
        results.insert(SimilarTerm(term, similarityScore, curve.clone()))
      }
    }
    results.done
  }
}

