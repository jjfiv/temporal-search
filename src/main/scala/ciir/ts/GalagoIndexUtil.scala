package ciir.ts

import org.lemurproject.galago.core.index.Index
import org.lemurproject.galago.core.index.ValueIterator
import org.lemurproject.galago.core.retrieval.iterator.MovableCountIterator
import org.lemurproject.galago.core.retrieval.ScoredDocument
import org.lemurproject.galago.core.retrieval.LocalRetrieval
import org.lemurproject.galago.core.retrieval.processing.ScoringContext
import org.lemurproject.galago.tupleflow.Parameters

object GalagoIndexUtil {
  def retrievalFromPath(indexPath: String): LocalRetrieval = {
    var parms = new Parameters()
    new LocalRetrieval(indexPath, parms)
  }

  def forKeyInIndex(index: Index, indexPartName: String, block: (String,MovableCountIterator)=>Unit) {
    var indexPartReader = index.getIndexPart(indexPartName)
    if(indexPartReader == null) { return }

    var keyIter = indexPartReader.getIterator

    while(!keyIter.isDone) {
      val str = keyIter.getKeyString
      var valueIter = keyIter.getValueIterator.asInstanceOf[MovableCountIterator]
      valueIter.setContext(new ScoringContext)
      block(str, valueIter)
      keyIter.nextKey
    }
  }
  
  def forDocInInvertedList(iter: MovableCountIterator, block: (Int,Int)=>Unit) {
    while(!iter.isDone) {
      val doc = iter.currentCandidate
      iter.getContext.document = doc
      val count = iter.count()
      block(doc, count)
      iter.movePast(doc)
    }
  }

  def forDocInIndex(index: Index, block: Int=>Unit) {
    var lenIter = index.getLengthsIterator

    while(!lenIter.isDone) {
      val id = lenIter.currentCandidate()
      block(id)
      lenIter.movePast(id)
    }
  }

  def forDocLenInIndex(index: Index, block: (Int,Int)=>Unit) {
    var lenIter = index.getLengthsIterator
    var context = new ScoringContext
    lenIter.setContext(context)

    while(!lenIter.isDone) {
      val id = lenIter.currentCandidate
      context.document = id
      lenIter.syncTo(id)

      block(id, lenIter.getCurrentLength)
      lenIter.movePast(id)
    }
  }
}
