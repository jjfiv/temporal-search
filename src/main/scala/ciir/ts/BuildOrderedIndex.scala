package ciir.ts

// import subprojects
import org.lemurproject.galago.{core => GalagoCore}
import org.lemurproject.galago.{tupleflow => Tupleflow}
// import memory index interface
import GalagoCore.index.mem._
// import corpusfilewriter
import GalagoCore.index.corpus.CorpusFileWriter
import GalagoCore.index.disk.{DiskIndex, PositionIndexWriter}
import GalagoCore.retrieval.processing.ScoringContext
import GalagoCore.index.IndexPartReader
import GalagoCore.parse.Document
import Tupleflow.{Parameters, FakeParameters}
import java.util.{List => JList, HashMap => JHashMap, ArrayList => JArrayList, Arrays => JArrays}

// this class generalizes the method of writing to disk parts of indices
sealed trait IndexPartStream {
  def addDocument(doc: Document)
  def flushToDisk()
}
class CorpusStream(path: String) extends IndexPartStream {
  var corpusWriter = {
    var corpusParms = new Parameters
    corpusParms.set("filename", path)
    new CorpusFileWriter(new FakeParameters(corpusParms))
  }
  override def addDocument(doc: Document) { corpusWriter.process(doc) }
  override def flushToDisk() { corpusWriter.close() }
}
class MemPartStream(part: MemoryIndexPart, path: String) extends IndexPartStream {
  override def addDocument(doc: Document) { part.addDocument(doc) }
  override def flushToDisk() { part.flushToDisk(path) }
}

// Modeled after GalagoCore.index.mem.MemoryIndexTest
object BuildOrderedIndex {
  def buildDoc(num: Int, path: String): Document = {
    import collection.JavaConversions._
    
    val metadata = XMLStream.simpleGetKeys(path, Set("identifier","title", "date", "year", "creator", "author"))
    val words = {
      val normaled = MBTEI.words(path).map(_.toLowerCase.filter(_.isLetterOrDigit))
      normaled.filter(!_.isEmpty)
    }

    var d = new Document()
    d.name = metadata.getOrElse("identifier","doc-"+num)
    d.text = words.mkString(" ")
    d.terms = words.toSeq
    d.tags = new JArrayList()
    d.metadata = new JHashMap(metadata)
    d.identifier = num
    
    d
  }

  def readDoc(num: Int, path: String): Document = {
    import collection.JavaConversions._
    val (metadata, words, text) = MBTEI.parse(path, true, true, true)

    var d = new Document
    d.name = metadata.getOrElse("identifier", "doc-"+num)
    d.text = text
    d.terms = words.toSeq
    d.tags = new JArrayList()
    d.metadata = new JHashMap(metadata)
    d.identifier = num

    d
  }

  def makeIndexPart(args: Array[String]) {
    if(args.size != 3 && args.size != 4) {
      val allowedParts = Array("corpus","names","lengths","extents","postings","postings.porter")
      Util.quit(
        "expected arguments: listFile indexPartName indexDir [documentNumber]\n" +
        allowedParts.mkString("  indexPartName = {",", ","}")
        )
    }

    val documentList = IO.fileLines(args(0))
    val partName = args(1)
    val indexDir = args(2)
    val docNumStart = if(args.size == 4) args(3).toInt else 0

    // check output directory at start:
    IO.requireDirectory(indexDir)

    var index: IndexPartStream = {
      val path = {
        var base = indexDir+"/"+partName
        if(docNumStart != 0)
          base+"-"+docNumStart
        else
          base
      }
      partName match {
        case "corpus" => new CorpusStream(path)
        case "names" =>
          new MemPartStream(new MemoryDocumentNames(new Parameters), path)
        case "lengths" =>
          new MemPartStream(new MemoryDocumentLengths(new Parameters), path)
        case "extents" =>
          new MemPartStream(new MemoryWindowIndex(new Parameters), path)
        case "postings" =>
          new MemPartStream(new MemoryPositionalIndex(new Parameters), path)
        case "postings.porter" => {
          var parms = new Parameters
          parms.set("stemmer", classOf[GalagoCore.parse.stem.Porter2Stemmer].getName)
          new MemPartStream(new MemoryPositionalIndex(parms), path)
        }
        case other => {
          throw new UnsupportedOperationException("Don't know how to make a \""+other+"\" indexPart")
        }
      }
    }

    documentList.zipWithIndex.foreach{
      case (path, idx) => {
        val num = docNumStart + idx
        println("doc "+num+" "+path+" | "+Util.MiB(Util.usedMem)+" MiB")
        index.addDocument(buildDoc(num, path))
      }
    }
    
    println("Begin flushing index...")
    index.flushToDisk()
    println("Complete!")
  }

  def concatPostings(args: Array[String]) {
    if(args.size < 2) {
      Util.quit("error: expected arguments output-file input-files...")
    }
    val outputName = args(0)
    val inputNames = args.tail

    var outputWriter = {
      var parms = new Parameters
      parms.set("filename", outputName)
      new PositionIndexWriter(new FakeParameters(parms))
    }

    var readers = inputNames.map(path => DiskIndex.openIndexComponent(path).asInstanceOf[IndexPartReader])
    var iterators = readers.map(_.getIterator)

    // while there is an iterator with more keys...
    while(!iterators.isEmpty) {
      val currentKey = iterators.map(_.getKeyString).min
      var currentIters = iterators.filter(_.getKeyString == currentKey)

      println("process "+currentKey+" "+currentIters.size)
      // start entry for this word
      outputWriter.processWord(currentIters(0).getKey)

      currentIters.foreach(iter => {
        var cIter = iter.getValueIterator.asInstanceOf[Galago.PostingsIter]
        var ctx = new ScoringContext
        cIter.setContext(ctx)
        while(!cIter.isDone) {
          val doc = cIter.currentCandidate
          ctx.document = doc

          outputWriter.processDocument(doc)
          val extents = cIter.extents()
          for(idx <- 0 until extents.size()) {
            outputWriter.processPosition(extents.begin(idx))
          }

          cIter.movePast(doc)
        }
        
        // step this iterator to its next key
        iter.nextKey()
        if(iter.isDone) {
          println("our current iter finished!")
        }
      })

      // remove newly finished iterators
      iterators = iterators.filter(!_.isDone)
    }


    outputWriter.close()
    assert(iterators.forall(_.isDone))
  }
}

object Throwaway {
  def fixNumbers(args: Array[String]) {
    if(args.size != 1) {
      Util.quit("error: expected input-file")
    }
    val inputName = args(0)
    val outputName = inputName+"_fixed"

    var outputWriter = {
      var parms = new Parameters
      parms.set("filename", outputName)
      new PositionIndexWriter(new FakeParameters(parms))
    }

    def fixDoc(x: Int) = {
      val bucket = x / 1500
      val relpos = x % 1500
      val result = (bucket * 1000) + relpos
      assert(result >= 0 && result <= 10000)
      result
    }

    val reader = DiskIndex.openIndexComponent(inputName).asInstanceOf[IndexPartReader]
    var iterator = reader.getIterator

    while(!iterator.isDone) {
      outputWriter.processWord(iterator.getKey)
      println("fix "+iterator.getKeyString)

      var cIter = iterator.getValueIterator.asInstanceOf[Galago.PostingsIter]
      var ctx = new ScoringContext
      cIter.setContext(ctx)
      while(!cIter.isDone) {
        val doc = cIter.currentCandidate
        ctx.document = doc

        outputWriter.processDocument(fixDoc(doc))
        val extents = cIter.extents()
        for(idx <- 0 until extents.size()) {
          outputWriter.processPosition(extents.begin(idx))
        }
        cIter.movePast(doc)
      }
      
      iterator.nextKey()
    }

    outputWriter.close()
  }
}
