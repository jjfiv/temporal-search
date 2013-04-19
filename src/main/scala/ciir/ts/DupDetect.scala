package ciir.ts

sealed case class HashedDoc(val index: Int, val data: Array[Int]) {
  override def toString = {
    index+":"+data.take(10).map(Util.hex(_)).mkString(", ")
  }
  def ==(other: HashedDoc): Boolean = {
    if(index != other.index || data.size != other.data.size) {
      return false
    }
    data.zip(other.data).foreach {
      case (x,y) => if (x != y) return false
    }
    true
  }
}

object MBTEIDoc {
  def read(path: String): MBTEIDoc = {
    if(!IO.fileExists(path)) {
      return MBTEIDoc("", "", Array())
    }
    val metadata = XMLStream.simpleGetKeys(path, Set("title"))
    
    //give up on empty titles
    val title = metadata.getOrElse("title", "").trim.toLowerCase
    if(title.isEmpty) {
      return MBTEIDoc("", "", Array())
    }

    val data = MBTEI.words(path).map(_.hashCode)

    MBTEIDoc(path, title, data)
  }
}

sealed case class MBTEIDoc(val path: String, val title: String, val data: Array[Int]) {
  def size = data.size
  def similar(other: MBTEIDoc): Boolean = {
    title == other.title || DupDetect.similar(data, other.data)
  }
}

object LCS {
  val ComputeLimit = 2000
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
  def readDocument(idx: Int, path: String): HashedDoc = {
    if(!IO.fileExists(path)) {
      Console.err.println("file "+path+" does not exist!")
    }
    HashedDoc(idx, MBTEI.words(path).map(_.hashCode))
  }
  
  private def loadBooks(listFile: String, startIndex: Int, count: Int) = {
    val bookIds = (startIndex until (startIndex+count))
    val books = IO.linesFromFile(listFile, startIndex, count).zip(bookIds)
    
    if(books.size == 0) {
      Util.quit("Couldn't find any books in list file \""+listFile+"\"!")
    }

    var parsed = books.flatMap {
      case (bookPath, idx) => {
        if(IO.fileExists(bookPath)) {
          Some(readDocument(idx, bookPath))
        } else {
          Console.err.println("# couldn't find book <"+bookPath.trim+">")
          None
        }
      }
    }
    parsed
  }

  val RandomSeed = 0xdeadbeef

  def sampleBooks(args: Array[String]) {
    if(args.size != 2) {
      Util.quit("expected arguments: inputList targetNum")
    }

    // shuffle the input list pseudorandomly
    val inputList = {
      var rand = new util.Random(RandomSeed)
      rand.shuffle(IO.fileLines(args(0)).toSeq)
    }
    val targetNum = args(1).toInt
    val outList = args(0)+".dedup"
    val outBarrel = args(0)+".barrel.gz"



    if(inputList.isEmpty) {
      Util.quit("no documents in inputList")
    }

    var uniqueDocuments = new collection.mutable.ArrayBuffer[MBTEIDoc]

    uniqueDocuments += MBTEIDoc.read(inputList(0))
    var curDocIdx = 1
    var tossed = 0
    
    while(curDocIdx < inputList.size && uniqueDocuments.size < targetNum) {
      val curDoc = MBTEIDoc.read(inputList(curDocIdx))


      if(curDocIdx % 10 == 0) {
        println("# "+curDocIdx+" "+uniqueDocuments.size+"/"+targetNum)
      }

      // invalid documents become zero-length
      if(curDoc.size != 0) {

        // for all uniqueDocuments so far 
        // if there does not exist a document similar to this new one, keep it
        if(!uniqueDocuments.exists(_.similar(curDoc))) {
          uniqueDocuments += curDoc
        } else {
          tossed += 1
        }
      
      }
      curDocIdx+=1
    }

    Console.err.println("# kept "+uniqueDocuments.size+" documents")
    Console.err.println("# gave up on "+tossed+" documents")

    val paths = uniqueDocuments.map(_.path)
    val hdocs = uniqueDocuments.zipWithIndex.map {
      case (doc, idx) => HashedDoc(idx, doc.data)
    }

    IO.printToFile(outList, fp => {
      paths.foreach(fp.println(_))
    })
    saveBarrel(outBarrel, hdocs)
  }

  private def saveBarrel(outFile: String, docs: IndexedSeq[HashedDoc]) {
    var fp = IO.binaryOutputStream(outFile);

    try {
      // num documents
      fp.writeInt(docs.size)
      
      docs.foreach {
        case HashedDoc(idx, data) => {
          fp.writeInt(idx)
          fp.writeInt(data.size)
          data.foreach(fp.writeInt)
        }
      }
    } finally {
      fp.close()
    }
  }

  private def readDocFromBarrel(fp: java.io.DataInputStream) = {
    val docIdx = fp.readInt
    val docLen = fp.readInt
    var data = new Array[Int](docLen)
    data.indices.foreach {
      data(_) = fp.readInt
    }
    HashedDoc(docIdx, data)
  }

  private def readBarrel(inFile: String): Array[HashedDoc] = {
    var fp = IO.binaryInputStream(inFile)

    try {
      val numDocs = fp.readInt
      var docs = new Array[HashedDoc](numDocs)
      docs.indices.foreach {
        docs(_) = readDocFromBarrel(fp)
      }
      return docs
    } finally {
      fp.close()
    }
    
    // only if we had an error
    Array()
  }

  def compareBarrels(args: Array[String]) {
    if(args.size != 2) {
      Util.quit("Expected args: first-barrel second-barrel")
    }
    val streamingBarrel = IO.binaryInputStream(args(0))
    val barrel1 = readBarrel(args(1))

    val listBName = {
      val name = args(1)
      if(name.endsWith(".barrel.gz")) {
        name.substring(0,name.lastIndexOf(".barrel.gz"))
      } else {
        name
      }
    }
    
    var duplicates = new gnu.trove.set.hash.TIntHashSet

    try {
      val numDocs = streamingBarrel.readInt

      // only load one book at a time from one document
      Util.loopUntil(numDocs)(nvm => {
        val bookA = readDocFromBarrel(streamingBarrel)
        
        barrel1.foreach(bookB => {
          if(!duplicates.contains(bookB.index)) {
            if(similar(bookA.data, bookB.data)) {
              duplicates.add(bookB.index)
              // output barrel name and 
              println(listBName+" "+bookB.index)
            }
          }
        })
      })
    } finally {
      streamingBarrel.close()
    }

  }

  def similar(docA: Array[Int], docB: Array[Int]): Boolean = {
    // if the difference in their sizes is greater than half of the smaller document; they're definitely not the same
    if( (math.abs(docA.size - docB.size)/math.min(docA.size, docB.size)) > .5) {
      return false
    }
    val wordsA = docA.toSet
    val wordsB = docB.toSet
    val commonWords = wordsA intersect wordsB

    val overlap = {
      val uniqueA = wordsA.size
      val uniqueB = wordsB.size

      val maxUnique = Seq(uniqueA, uniqueB).max

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

    val orderedUniqA = docA.filter(commonWords.contains)
    val orderedUniqB = docB.filter(commonWords.contains)

    val orderedLength = Seq(orderedUniqA.size, orderedUniqB.size, maxLcsLen).min
    val lcsLen = LCS.run(orderedUniqA, orderedUniqB)

    val lcsFrac = Util.fraction(lcsLen, orderedLength)

    lcsFrac > .50
  }
}

