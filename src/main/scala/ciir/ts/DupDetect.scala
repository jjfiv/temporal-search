package ciir.ts

object DupDetect {
  def genBarrels(args: Array[String]) {
    if(args.size != 3) {
      Util.quit("Expected args: list-file startIndex count")
    }
    val fileName = args(0)
    val startIndex = args(1).toInt
    val count = args(2).toInt

    val books = IO.linesFromFile(fileName, startIndex, count)
    val bookIds = (startIndex until (startIndex+count))

    if(books.size == 0) {
      Util.quit("Couldn't find any books in list file \""+fileName+"\"!")
    }

    // read each book
    val parsed = books.zip(bookIds).map{
      case (bookPath, id) => {
        val hashed = MBTEI.words(bookPath).map(_.hashCode)
        (id, hashed)
      }
    }

    parsed.foreach {
      case (id, data) => {
        println(id +": "+data.take(10).map(Util.hex(_)).mkString(", "))
      }
    }
  }
}

