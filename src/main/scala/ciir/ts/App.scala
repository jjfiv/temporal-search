package ciir.ts

object IndexInspector {
  def run(args: Array[String]) {
    args.map(indexPath => {
      val retrieval = GalagoIndexUtil.retrievalFromPath(indexPath)

      println("Inspecting index \""+indexPath+"\"")
      println("  Document Count: "+retrieval.getCollectionStatistics("#lengths:document:part=lengths()").documentCount.toInt)
      println("  Available Parts: "+retrieval.getAvailableParts.toPrettyString)
    })
  }
}

object App {
  var actions = Map[String,Array[String]=>Unit](
    ("inspect" -> IndexInspector.run),

    ("term-plot-dates" -> DateExplorer.plotTerms),

    ("dates-build" -> DateExplorer.buildIndex),
    ("dates-kmeans" -> DateExplorer.kmeansTerms),

    ("barrel-gen" -> DupDetect.genBarrel),
    ("barrel-cmp" -> DupDetect.compareBarrels),

    ("count-books" -> CountBooksByDate.run),
    ("count-books-graph" -> CountBooksByDate.graph),
    ("count-books-stat" -> CountBooksByDate.stats),
    ("count-books-by-date" -> CountBooksByDate.dateLists)
    )

  def printAvailable() {
    println("  try one of: "+ actions.keys.mkString(", "))
    println("")
  }

  def main(args: Array[String]) {
    println(classOf[ciir.proteus.galago.parse.MBTEIBookParser].getName)
    if(args.size == 0) {
      println("Expected at least one argument to determine task.")
      printAvailable()
      return
    }
    
    val name = args.head
    
    actions.get(name) match {
      case Some(action) => {
        action(args.tail)
      }
      case None => {
        println("No matching action for \""+name+"\"\n")
        printAvailable()
      }
    }

  }
}

