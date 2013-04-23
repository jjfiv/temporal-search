package ciir.ts

import gnu.trove.map.hash._
import collection.mutable.ArrayBuilder

class LocalDateInfo(val retrieval: Galago.Retrieval) {
  var cache = new TIntIntHashMap
  
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
    if(cache.containsKey(doc)) {
      cache.get(doc)
    } else {
      val date = retrieveDate(doc)
      cache.put(doc, date)
      date
    }
  }
}

class DateRetrieval(indexDir: String) {
  val retrieval = Galago.openRetrieval(indexDir)
  val index = new BasicIndex(retrieval.getIndex)
  val dateInfo = new LocalDateInfo(retrieval)

  def numDocs = index.numDocs
  def getDocName(id: Int) = index.getDocName(id)

  def dateSearch(query: String): Array[DocDateScore] = {
    Galago.doCountsQuery(retrieval, numDocs, query).flatMap {
      case DocCount(doc, count) => {
        val date = dateInfo.getDate(doc)
        if(date >= 1820 && date <= 1919) {
          Some(DocDateScore(doc, date, count))
        } else None
      }
    }
  }
  def search(query: String): Array[Long] = {
    var tfVector = new Array[Long](numDocs)
    Galago.doCountsQuery(retrieval, numDocs, query).foreach {
      case DocCount(doc, count) => tfVector(doc) = count
    }
    tfVector
  }
  def toDateVector(tfVector: Array[Long]): Array[Long] = {
    assert(tfVector.size == numDocs)
    var dateTF = new Array[Long](100)
    tfVector.zipWithIndex.foreach {
      case (count, doc) => {
        val date = dateInfo.getDate(doc)
        if(date >= 1820 && date <= 1919) {
          dateTF(date - 1820) += count
        }
      }
    }
    dateTF
  }
}

case class DocDateScore(val docId: Int, val date: Int, val score: Long) { }
case class SimilarTerm(val key: String, val score: Double, val data: Array[Long]) extends Ordered[SimilarTerm] {
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
    var tfVector = new ArrayBuilder.ofLong
    Galago.lengths(index.getLengthsIterator)(len => {
      size += 1
      tfVector += len
    })
    (size, tfVector.result())
  }

  def getDocName(id: Int) = index.getName(id)
  def getLength(id: Int) = maxTFVector(id)

  def eachPosting(op: (String,Array[Long])=>Unit) {
    var keyIter = postings.getIterator
    
    Util.timed("iterate over all counts:", {
      Galago.keys(keyIter) {
        var data = new Array[Long](numDocs)
        
        // read in posting list, accumulate counts
        var cIter = keyIter.getValueIterator.asInstanceOf[Galago.CountsIter]
        Galago.docs(cIter) { data(_) = cIter.count() }
        
        // handle this posting
        op(keyIter.getKeyString, data)
      }
    })
  }

  def cosineSimilarity(as: Array[Long], bs: Array[Long]): Double = {
    assert(as.size == bs.size)
    assert(as.size == maxTFVector.size)

    var dot = 0.0
    var magA = 0.0
    var magB = 0.0

    var idx = 0
    while(idx < as.size) {
      val ai = as(idx).toDouble
      val bi = bs(idx).toDouble
      
      dot += ai*bi
      magA += ai*ai
      magB += bi*bi

      idx += 1
    }

    dot / (math.sqrt(magA)*math.sqrt(magB))
  }

  def findSimilar(queryCurve: Array[Long], numResults: Int): Array[SimilarTerm] = {
    var results = new RankedList[SimilarTerm](numResults)
    eachPosting {
      case (term, curve) => {
        val similarityScore = cosineSimilarity(curve, queryCurve)
        results.insert(SimilarTerm(term, similarityScore, curve))
      }
    }
    results.done
  }
}

