package ciir.ts

import gnu.trove.map.hash.{TIntIntHashMap, TIntLongHashMap}
import gnu.trove.set.hash.{TIntHashSet, TLongHashSet}
import collection.mutable.ArrayBuilder

import ciir.ts.index.GalagoIter

object Statistics {
  def mean(xs: Array[Long]) = {
    xs.sum.toDouble / xs.size.toDouble
  }
  def mean(xs: Array[Double]) = {
    xs.sum / xs.size
  }
  def median(xs: Array[Long]): Double = {
    val tmp = xs.sorted
    val mid = tmp.size/2
    if(tmp.size % 2 == 1) {
      tmp(mid)
    } else {
      (tmp(mid-1) + tmp(mid)) / 2
    }
  }
  def variance(xs: Array[Long]) = {
    val mu = mean(xs)
    mean(xs.map(x =>{
      val off = x.toDouble - mu
      off*off
    }))
  }
  def maxIndex(xs: Array[Long]) = { 
    xs.zipWithIndex.maxBy(_._1)._2
  }
  def minIndex(xs: Array[Long]) = { 
    xs.zipWithIndex.minBy(_._1)._2
  }
  
  def summary(data: Array[Long], base: Int=0) = {
    println("  total: "+data.sum)
    println("  max: "+data.max+" in "+(base+maxIndex(data)))
    println("  mean: "+mean(data))
    println("  variance: "+variance(data))
    println("  median: "+median(data))
    println("  min: "+data.min+" in "+(base+minIndex(data)))
  }
}

case class DocDateScore(val docId: Int, val date: Int, val score: Long) { }
case class SimilarTerm(val key: String, val score: Double, val data: Array[Long]) { }

class RankedList(val numHits: Int) {
  private var results = new Array[SimilarTerm](numHits)
  private var count = 0

  def compare(a: SimilarTerm, b: SimilarTerm) = a.score > b.score

  def insert(newest: SimilarTerm) {
    if(count < numHits) {
      results(count) = newest
      count += 1
      return
    }

    // discard new things with tiny scores, special case
    if(compare(results.last, newest)) {
      return
    }

    // chop current array in pieces and reassemble
    val (better, worse) = results.partition(compare(_,newest))
    results = (better :+ newest) ++ worse.dropRight(1)
  }

  def done = {
    if (count <= numHits) {
      results.take(count).sortBy(_.score).reverse
    } else {
      results.reverse
    }
  }
}

class BasicIndex(var index: GalagoIter.Index) {
  val postings = index.getIndexPart("postings")
  val counts = index.getIndexPart("just-counts")
  val (numDocs, maxTFVector) ={
    var size = 0
    var tfVector = new ArrayBuilder.ofLong
    GalagoIter.lengths(index.getLengthsIterator)(len => {
      size += 1
      tfVector += len
    })
    (size, tfVector.result())
  }

  def eachPosting(op: (String,Array[Long])=>Unit) {
    var keyIter = counts.getIterator
    
    Util.timed("iterate over all counts:", {
      GalagoIter.keys(keyIter) {
        var data = new Array[Long](numDocs)
        
        // read in posting list, accumulate counts
        var cIter = keyIter.getValueIterator.asInstanceOf[GalagoIter.Counts]
        GalagoIter.docs(cIter) { data(_) = cIter.count() }
        
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
    var results = new RankedList(numResults)
    eachPosting {
      case (term, curve) => {
        val similarityScore = cosineSimilarity(curve, queryCurve)
        results.insert(SimilarTerm(term, similarityScore, curve))
      }
    }
    results.done
  }
}


object SimilarTerms {
  def cli(args: Array[String]) {
    if(args.size != 1 || !IO.dirExists(args(0))) {
      Util.quit("expected args: galago-index-dir")
    }
    val indexFolder = args(0)
    val retrieval = GalagoIndexUtil.retrievalFromPath(indexFolder)
    val index = new BasicIndex(retrieval.getIndex)
    val numDocs = index.numDocs
    
    val dateInfo = new LocalDateInfo(retrieval)

    Util.runCLI("enter-query[quit]: ","quit")(query => {
      println("you entered '"+query+"'")
      val dds = RawSearch.runQuery(retrieval, numDocs, query).flatMap(sdoc => {
        val date = dateInfo.getDate(sdoc.document)
        if(date >= 1820 && date <= 1919) {
          Some(DocDateScore(sdoc.document, date, sdoc.score.toLong))
        } else None
      })
      
      val dateMap = dds.groupBy(_.date)

      if(dateMap.nonEmpty) {
        println("years found: "+dateMap.keys.toBuffer.sorted.mkString(", "))

        Util.runCLI("inspect-results[done,year,similar,summary]: ", "done") {
          case "summary" => {
            var histogram = new Array[Long](100)
            dateMap.map {
              case (date, ddsForDate) => {
                histogram(date - 1820) = ddsForDate.map(_.score).sum
              }
            }
            Statistics.summary(histogram, 1820)
          }
          case "similar" => {
            var curve = new Array[Long](index.numDocs)
            dds.foreach {
              case DocDateScore(doc, _ , score) => curve(doc) = score
            }
            index.findSimilar(curve, 10) foreach {
              case SimilarTerm(key, score, data) => {
                println(key+" "+score)
              }
            }
          }
          case x => {
            val date = x.toInt
            dateMap.get(date) match {
              case Some(info) => Statistics.summary(info.map(_.score), date)
              case None => println("No results for "+date)
            }
          }
        }
        
        //(1820 to 1919).map(date => dateMap.get(date) match {
        //  case Some(info) => {
        //    println("year: "+date)
        //    println("  numDocs: "+info.size)
        //    val counts = info.map(_.score)
        //    Statistics.summary(counts)
        //  }
        //  case None=> { }
        //})
      }
      
      //println("date,count")
      //histogram.zipWithIndex.foreach {
        //case (count, idx) => println(1820+idx+","+count)
      //}
    })
  }
}

