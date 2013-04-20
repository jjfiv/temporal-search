package ciir.ts

// import subprojects
import org.lemurproject.galago.{core => GalagoCore}
import org.lemurproject.galago.{tupleflow => GalagoTupleflow}
// import memory index interface
import GalagoCore.index.mem.{MemoryIndex, FlushToDisk}
// import corpusfilewriter
import GalagoCore.index.corpus.CorpusFileWriter
import GalagoCore.parse.Document
import GalagoTupleflow.Parameters
import GalagoTupleflow.FakeParameters
import java.util.{List => JList, HashMap => JHashMap, ArrayList => JArrayList, Arrays => JArrays}


// Modeled after GalagoCore.index.mem.MemoryIndexTest
object BuildOrderedIndex {
  def buildDoc(num: Int, path: String): Document = {
    import collection.JavaConversions._
    
    val metadata = XMLStream.simpleGetKeys(path, Set("identifier","title", "date"))
    val words = MBTEI.words(path)

    var d = new Document()
    d.name = metadata.getOrElse("identifier","doc-"+num)
    d.text = words.mkString(" ")
    d.terms = words.toSeq
    d.tags = new JArrayList()
    d.metadata = new JHashMap(metadata)
    
    d
  }

  // make corpus separately from index
  def makeCorpus(args: Array[String]) {
    if(args.size != 2) {
      Util.quit("Expected args: documentList corpusFile")
    }
    val documentList = IO.fileLines(args(0))
    val corpusFile = args(1)
    var corpusWriter = {
      var corpusParms = new Parameters
      corpusParms.set("filename", corpusFile)
      new CorpusFileWriter(new FakeParameters(corpusParms))
    }

    documentList.zipWithIndex.foreach{
      case (path, idx) => {
        println("doc "+idx+" "+path+" ["+Util.MiB(Util.usedMem)+"] MiB")
        corpusWriter.process(buildDoc(idx, path))
      }
    }
    corpusWriter.close()
  }
  def makeIndex(args: Array[String]) {
    if(args.size != 2) {
      Util.quit("Expected args: documentList outputIndex")
    }
    val documentList = IO.fileLines(args(0))
    val outDir = args(1)
    
    var index = {
      var parms = new Parameters
      parms.set("makecorpus", false) // corpus is separate task
      new MemoryIndex(new FakeParameters(parms))
    }
    assert(!index.containsPart("corpus"))

    documentList.foreach(path => {
      val idx = index.documentsInIndex
      println("doc "+idx+" "+path+" ["+Util.MiB(Util.usedMem)+"] MiB")
      index.process(buildDoc(idx, path))
    })

    val totalDocs = index.documentsInIndex
    println("Read "+totalDocs)
    
    Util.suggestGC()
    println("Used memory: "+Util.MiB(Util.usedMem)+"MiB")

    println("Finishing index write...")
    val indexWriter = (new FlushToDisk)
    indexWriter.flushMemoryIndex(index, outDir)
    println("Done!")
  }
}
