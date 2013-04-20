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

    ("barrel-cmp" -> DupDetect.compareBarrels),
    ("sample-book-list" -> DupDetect.sampleBooks),

    ("count-books" -> CountBooksByDate.run),
    ("count-books-by-date" -> CountBooksByDate.dateLists),

    // build a part of a memory index
    // separated for memory concerns; preserves input list order
    ("build-index-part" -> BuildOrderedIndex.makeIndexPart),
    
    // swing-based gui for histogram stuff
    ("gui" -> ciir.ts.gui.UIMain.launch),
    
    // build a memory index
    // separated for memory concerns
    ("build-ordered-index" -> BuildOrderedIndex.makeIndex),
    ("build-ordered-corpus" -> BuildOrderedIndex.makeCorpus),
>>>>>>> gui

    // invoke the galago main()
    ("invoke-galago" -> org.lemurproject.galago.core.tools.App.run)
    )

  def printAvailable() {
    println("  try one of: "+ actions.keys.mkString(", "))
    println("")
  }

  def main(args: Array[String]) {
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

