package ciir.ts

// import subprojects
import org.lemurproject.galago.{core => GalagoCore}
import org.lemurproject.galago.{tupleflow => GalagoTupleflow}
// import memory index interface
import GalagoCore.index.mem._
// import corpusfilewriter
import GalagoCore.index.corpus.CorpusFileWriter
import GalagoCore.parse.Document
import GalagoTupleflow.Parameters
import GalagoTupleflow.FakeParameters
import java.util.{List => JList, HashMap => JHashMap, ArrayList => JArrayList, Arrays => JArrays}

// this class generalizes the method of writing to disk parts of indices
sealed trait IndexPartStream {
  def addDocument(doc: Document)
  def flushToDisk()
}
class CorpusStream(path: String) extends IndexPartStream {
  var corpusWriter = {
    var corpusParms = new Parameters
    corpusParms.set("filename", path+"/corpus")
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
    
    val metadata = XMLStream.simpleGetKeys(path, Set("identifier","title", "date"))
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

  def makeIndexPart(args: Array[String]) {
    if(args.size != 3) {
      val allowedParts = Array("corpus","names","lengths","extents","postings","postings.porter")
      Util.quit(
        "expected arguments: listFile indexPartName indexDir [postings-chars]\n" +
        allowedParts.mkString("  indexPartName = {",", ","}")
        )
    }

    val documentList = IO.fileLines(args(0))
    val partName = args(1)
    val indexDir = args(2)

    // check output directory at start:
    IO.requireDirectory(indexDir)

    var index: IndexPartStream = {
      val path = indexDir+"/"+partName
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
        println("doc "+idx+" "+path+" | "+Util.MiB(Util.usedMem)+" MiB")
        index.addDocument(buildDoc(idx, path))
      }
    }
    
    println("Begin flushing index...")
    index.flushToDisk()
    println("Complete!")
  }
}
