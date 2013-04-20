package ciir.ts

// import subprojects
import org.lemurproject.galago.{core => GalagoCore}
import org.lemurproject.galago.{tupleflow => GalagoTupleflow}
// import memory index interface
import GalagoCore.index.mem.{MemoryIndex, FlushToDisk}
import GalagoCore.parse.Document
import GalagoTupleflow.Parameters
import GalagoTupleflow.FakeParameters
import java.util.{List => JList, HashMap => JHashMap, ArrayList => JArrayList, Arrays => JArrays}



object BuildOrderedIndex {
  def run(args: Array[String]) {
    if(args.size != 2) {
      Util.quit("Expected args: documentList outputIndex")
    }
    val runtime = Runtime.getRuntime() // for profiling
    val documentList = IO.fileLines(args(0))
    val outDir = args(1)
    var parms = new Parameters
    parms.set("makecorpus", true)
    var index = new MemoryIndex(new FakeParameters(parms))

    var d = new Document()
    documentList.foreach(path => {
      import collection.JavaConversions._
      val metadata = XMLStream.simpleGetKeys(path, Set("identifier","title", "date"))
      val words = MBTEI.words(path)
      val docNum = index.documentsInIndex

      println("doc "+docNum+" "+path)
      val usedMem = runtime.totalMemory() - runtime.freeMemory()
      println("Used memory: "+usedMem+"B "+usedMem/1024+"kB "+usedMem/(1024*1024)+"MB")
      d.name = metadata.getOrElse("identifier","doc-"+docNum)
      d.text = words.mkString(" ")
      d.terms = words.toSeq
      d.tags = new JArrayList()
      d.metadata = new JHashMap(metadata)

      index.process(d)
    })

    runtime.gc()
    runtime.gc()
    val usedMem = runtime.totalMemory() - runtime.freeMemory()
    println("Used memory: "+usedMem+"B "+usedMem/1024+"kB "+usedMem/(1024*1024)+"MB")
    println("Writing "+index.documentsInIndex+ " documents to disk.")

    val indexWriter = (new FlushToDisk)
    indexWriter.flushMemoryIndex(index, outDir)

  }
}
