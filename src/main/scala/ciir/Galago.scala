package ciir.ts

import org.lemurproject.galago.{core => GalagoCore}
import org.lemurproject.galago.{tupleflow => Tupleflow}

import GalagoCore.retrieval.LocalRetrieval
import GalagoCore.retrieval.iterator._
import GalagoCore.index.{KeyIterator, ValueIterator}
import GalagoCore.retrieval.processing.ScoringContext

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
}


