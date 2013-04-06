package ciir.ts
object IndexInspector {
  def run(args: Array[String]) {
    args.map(indexPath => {
      val retrieval = GalagoIndexUtil.retrievalFromPath(indexPath)

      println("Inspecting index \""+indexPath+"\"")
      println("  Document Count: "+retrieval.getCollectionStatistics("#lengths:document:part=lengths()").documentCount.toInt)
    })
  }
}

object App {
  var actions = Map[String,Array[String]=>Unit](
    ("inspect" -> IndexInspector.run)
    )

  def main(args: Array[String]) {
    val name = args.head
    
    actions.get(name) match {
      case Some(action) => {
        action(args.tail)
      }
      case None => {
        println("No matching action for \""+name+"\"\n")
        println("  try one of: "+ actions.keys.mkString(", "))
        println("")
      }
    }

  }
}

