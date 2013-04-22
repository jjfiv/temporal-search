package ciir.ts

import org.lemurproject.galago.{core => GalagoCore}
import org.lemurproject.galago.{tupleflow => GalagoTupleflow}

import GalagoCore.scoring._
import GalagoCore.retrieval._
import query.{Node, StructuredQuery, NodeParameters}
import iterator._
import GalagoTupleflow.Parameters

// implement our custom scoring
class RawScoreIter
(p: NodeParameters, ls: MovableLengthsIterator, it: MovableCountIterator)    
extends ScoringFunctionIterator(p, ls, it) {
  super.setScoringFunction(new ScoringFunction {
    def score(count: Int, length: Int): Double = count.toDouble
  })
  val maxTF = getMaxTF(p, it)
  override def minimumScore = 0.0
  override def maximumScore = maxTF
}

object RawSearch {
  def parseQuery(request: String): Node = {
    // drop anything not a letter or a digit
    val cleaned = request.filter(ch => {
      ch.isLetterOrDigit || ch.isWhitespace
    })
    val iterName = classOf[RawScoreIter].getName()
    StructuredQuery.parse("#feature:class="+iterName+"("+cleaned+")")
  }
  
  def runQuery(retrieval: Retrieval, count: Int, query: String) = {
    val searchParms = new Parameters
    searchParms.set("requested", count)
    
    val queryTree = retrieval.transformQuery(parseQuery(query), searchParms)
    
    var scored = retrieval.runQuery(queryTree, searchParms)
    if(scored == null) {
      Array[ScoredDocument]()
    } else {
      scored
    }
  }

  
}
