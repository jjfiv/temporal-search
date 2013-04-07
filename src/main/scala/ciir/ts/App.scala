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
    ("count-books" -> CountBooksByDate.run)
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

