package ciir.ts

import org.lemurproject.galago.{core => GalagoCore}
import org.lemurproject.galago.{tupleflow => Tupleflow}

import GalagoCore.retrieval.LocalRetrieval
import GalagoCore.retrieval.iterator._
import GalagoCore.index.{KeyIterator, ValueIterator}
import GalagoCore.scoring.ScoringFunction
import GalagoCore.retrieval.processing.ScoringContext
import GalagoCore.retrieval.query.{Node, StructuredQuery, NodeParameters}

import Tupleflow.Parameters

object Galago {
  type Index = GalagoCore.index.Index
  type Retrieval = GalagoCore.retrieval.Retrieval

  def parameters(kv: Map[String,String]) = {
    new Parameters(collection.JavaConversions.mapAsJavaMap(kv))
  }

  def openRetrieval(path: String) = new LocalRetrieval(path, new Parameters)

  def keys(keyIter: KeyIterator)(op: =>Unit) {
    keyIter.reset()
    while(!keyIter.isDone) {
      op
      keyIter.nextKey
    }
  }

  def lengths(iter: MovableLengthsIterator)(op: Int=>Unit) {
    iter.reset()
    var ctx = new ScoringContext
    iter.setContext(ctx)
    while(!iter.isDone) {
      val doc = iter.currentCandidate
      ctx.document = doc
      op(iter.getCurrentLength)
      iter.movePast(doc)
    }
  }
  
  def docs(valueIter: ValueIterator)(op: Int=>Unit) {
    var iter = valueIter.asInstanceOf[MovableIterator]
    iter.reset()
    var ctx = new ScoringContext
    iter.setContext(ctx)
    while(!iter.isDone) {
      val doc = iter.currentCandidate
      ctx.document = doc
      op(doc)
      iter.movePast(doc)
    }
  }

  type CountsIter = MovableCountIterator with ValueIterator
  type ExtentsIter = MovableExtentIterator with ValueIterator
  type PostingsIter = CountsIter with ExtentsIter
  
  def parseCountsQuery(query: String): Node = {
    // drop anything not a letter or a digit
    val cleaned = query.filter(ch => {
      ch.isLetterOrDigit || ch.isWhitespace
    })
    val iterName = classOf[CountsScorer].getName()
    StructuredQuery.parse("#feature:class="+iterName+"("+cleaned+")")
    // optionally force part?
    //StructuredQuery.parse("#feature:class="+iterName+"(#lengths:document:part=lengths() #extents:"+cleaned+":part=postings() )")
  }

  def doCountsQuery(retrieval: Retrieval, count: Int, query: String): Array[DocCount] = {
    val searchParms = {
      var p = new Parameters
      p.set("requested", count)
      p
    }
    
    val request = retrieval.transformQuery(parseCountsQuery(query), searchParms)
    val scored = retrieval.runQuery(request, searchParms)

    //Console.err.println(request)
    
    if(scored == null) return Array()
    
    scored.map(sdoc => {
      DocCount(sdoc.document, sdoc.score.toInt)
    })
  }
}

case class DocCount(val doc: Int, val count: Int) { }

// implement custom scoring
class CountsScorer (p: NodeParameters, ls: MovableLengthsIterator, it: MovableCountIterator) extends ScoringFunctionIterator(p, ls, it) {
  super.setScoringFunction(new ScoringFunction {
    def score(count: Int, length: Int): Double = count.toDouble
  })
  val maxTF = getMaxTF(p, it)
  override def minimumScore = 0.0
  override def maximumScore = maxTF
}


