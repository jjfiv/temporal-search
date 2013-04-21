package ciir.ts.index

import org.lemurproject.galago.{core => GalagoCore}
import org.lemurproject.galago.{tupleflow => GalagoTupleflow}

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

  type Counts = MovableCountIterator with ValueIterator
  type Extents = MovableExtentIterator with ValueIterator
  type Postings = Counts with Extents
}

