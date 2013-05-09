package ciir.ts

// process:
// chooseFromList
// filterWords
// unix sort // faster than me doing it
// filterSorted
object TextIndex {
  def chooseFromList(args: Array[String]) {
    if(args.size != 2) {
      Util.quit("error: expected -> list-file count")
    }
    val inputFiles = IO.fileLines(args(0)).toSeq
    val count = args(1).toInt

    val rnd = new util.Random(13)
    val results = rnd.shuffle(inputFiles).take(count)

    results.foreach(println)
  }

  val stopWords ={
    import collection.JavaConversions._
    org.lemurproject.galago.core.util.WordLists.getWordList("inquery").toSet
  }

  def filterWords(args: Array[String]) {
    if(args.size != 3) {
      Util.quit("error: expected -> list-file worker numWorkers")
    }
    val myJob = {
      var inputFiles = IO.fileLines(args(0))
      val worker = args(1).toInt
      val numWorkers = args(2).toInt
      
      val numLinesEach = inputFiles.size / numWorkers
      val jobs = inputFiles.grouped(numLinesEach).toArray
      assert(jobs.size == numWorkers)
      jobs(worker)
    }


    // inlined MBTEI.words here
    myJob.foreach(path => {
      Console.err.println(path)
      var reader: MBTEIReader = null
      try {
        reader = new MBTEIReader(path)
        var done = false
        while(!done) {
          reader.nextWord() match {
            case None => { done = true }
            case Some(str) => { 
              val notGarbage = str.forall(ch => {
                val x = ch.toInt
                (x < 128 && x >= 32 && ch.isLetterOrDigit)
              })
              if(notGarbage) {
                // if it's a number, all should be
                val allWordOrNum =
                  str.forall(!_.isDigit) || str.forall(_.isDigit)
                val lower = str.toLowerCase
                if(!allWordOrNum) {
                } else if(lower.forall(_.isDigit) && lower.size != 4 && lower.size != 2) {
                  // drop anything that's not a four-digit date or a word
                } else if(stopWords.contains(lower)) {
                  // drop stopWords
                } else if(lower.size > 1) { // any single character is a stopword
                  println(lower)
                }
              }
            }
          }
        } // while
      } catch {
        case eof: java.io.EOFException => {
        }
      } finally {
        if(reader != null) {
          reader.close()
        }
      }
    })
  }

  def create(args: Array[String]) {
    if(args.size < 2) {
      Util.quit("error: expected -> idx1Data idx2Data...")
    }

    val Threshold = 600 // require a max of at least 600 ; utter heuristic about interesting words...

    val iters = args.zipWithIndex.map {
      case (fn, idx) => new SortedWordData(fn, idx)
    }

    val numSlots = iters.size
    var countData = new Array[Int](numSlots)
    
    while(iters.exists(_.hasWord)) {
      val rest = iters.filter(_.hasWord)

      val minWord = rest.minBy(_.word).word
      val matching = rest.filter(_.word == minWord)
      val bestCount = matching.map(_.count).max

      if(bestCount > Threshold) {
        java.util.Arrays.fill(countData, 0)
        iters.foreach(it => {
          countData(it.index) = it.count
        })
        
        println(minWord + " " + countData.mkString(" "))
      }
      // skip ahead
      matching.foreach(_.next)
    }
  }

}

class SortedWordData(path: String, val index: Int) {
  private var fp = IO.textInputStream(path)
  var word: String = null
  var count = 0
  next

  def hasWord = (word != null)
  def next = {
    val nextLine = fp.readLine
    if(nextLine == null) {
      word = null
    } else {
      nextLine.trim.split("\\s+") match {
        case Array(num, token) => {
          count = num.toInt
          word = token
        }
      }
    }
  }
}
