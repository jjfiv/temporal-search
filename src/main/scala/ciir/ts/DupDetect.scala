package ciir.ts

case class HashedDoc(val id: String, val index: Int, val data: Array[Int]) {
  def peek() {
    println(index+":"+id +": "+data.take(10).map(Util.hex(_)).mkString(", "))
  }
}

object LCS {
  val ComputeLimit = 2000
  def compare(a: String, b: String) = {
    def convert(x: String): Array[Int] = { 
      x.filter(!_.isLetterOrDigit).map(_.toLowerCase.toInt).toArray
    }

    val arrA = convert(a)
    val arrB = convert(b)

    run(convert(a), convert(b))
  }

  // http://en.wikipedia.org/wiki/Longest_common_subsequence_problem 
  def run(a: Array[Int], b: Array[Int]): Int = {
    def max(a: Int, b: Int) = if(a < b) b else a
    def min(a: Int, b: Int) = if(a > b) b else a
    val endA = min(ComputeLimit, a.size)
    val endB = min(ComputeLimit, b.size)

    var memo = Array.fill(endA+1, endB+1)(0)
    
    var ai = 0

    while(ai < endA) {
      var bi = 0
      while(bi < endB) {
        if(a(ai) == b(bi)) {
          memo(ai+1)(bi+1) = memo(ai)(bi)+1
        } else {
          memo(ai+1)(bi+1) = max(memo(ai+1)(bi), memo(ai)(bi+1))
        }
        bi+=1
      }
      ai+=1
    }
    memo(endA)(endB)
  }
}

object DupDetect {
  val MaxBookSize = 500000
  private def loadBooks(listFile: String, startIndex: Int, count: Int) = {
    val bookIds = (startIndex until (startIndex+count))
    val books = IO.linesFromFile(listFile, startIndex, count).zip(bookIds)
    
    if(books.size == 0) {
      Util.quit("Couldn't find any books in list file \""+listFile+"\"!")
    }

    var parsed = books.flatMap {
      case (bookPath, id) => {
        if(IO.fileExists(bookPath)) {
          val hashed = MBTEI.words(bookPath).map(_.hashCode)
          val bookID = MBTEI.idFromPath(bookPath)
          println(hashed.size)
          Some(HashedDoc(bookID, id, hashed))
        } else {
          Console.err.println("# couldn't find book <"+bookPath+">")
          None
        }
      }
    }
    parsed
  }
  def genBarrel(args: Array[String]) {
    if(args.size != 3) {
      Util.quit("Expected args: listFile startIndex count")
    }
    // read each book
    val books = loadBooks(args(0), args(1).toInt, args(2).toInt)
    
    if(books.size == 0) {
      Util.quit("Couldn't find any books")
    }
    
    var duplicates = new gnu.trove.set.hash.TIntHashSet

    for(idA <- 0 until (books.size - 1)) {
      val bookA = books(idA)
      if(!duplicates.contains(bookA.index)) {
        for(idB <- (idA + 1) until books.size) {
          val bookB = books(idB)
          if(!duplicates.contains(bookB.index)) {
            if(similar(bookA, bookB)) {
              println("# duplicate: "+(bookA.id, bookB.id))
              duplicates.add(bookB.index)
            }
          }
        }
      }
    }

    val uniqueDocuments = books.filter(bk => !duplicates.contains(bk.index))
    
    uniqueDocuments.foreach {
      case HashedDoc(id, _, data) => {
        println(id, data.take(1))
      }
    }
  }
  def abs(x: Int) = if(x > 0) x else -x
  def max(a: Int, b: Int) = if(a < b) b else a

  def similar(docA: HashedDoc, docB: HashedDoc): Boolean = {
    val wordsA = docA.data.toSet
    val wordsB = docB.data.toSet
    val commonWords = wordsA intersect wordsB

    val overlap = {
      val uniqueA = wordsA.size
      val uniqueB = wordsB.size

      val maxUnique = max(uniqueA, uniqueB)

      Util.fraction(commonWords.size,maxUnique)
    }

    // if the % of words common to these documents is higher than 85%, we don't want it
    if(overlap > .85) {
      return true
    }
    // don't bother with the extended checks if vocabulary is not 50% the same
    if(overlap < .50) {
      return false
    }
    
    val maxLcsLen = LCS.ComputeLimit

    val orderedUniqA = docA.data.filter(commonWords.contains)
    val orderedUniqB = docB.data.filter(commonWords.contains)

    val orderedLength = Array(orderedUniqA.size, orderedUniqB.size, maxLcsLen).min
    val lcsLen = LCS.run(orderedUniqA, orderedUniqB)

    val lcsFrac = Util.fraction(lcsLen, orderedLength)

    println (docA.id, docB.id, overlap, lcsFrac)

    lcsFrac > .50
  }
}

