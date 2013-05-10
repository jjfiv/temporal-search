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

  def cosineSimilarity(as: Array[Double], bs: Array[Double]): Double = {
    assert(as.size == bs.size)

    var dot = 0.0
    var magA = 0.0
    var magB = 0.0

    var idx = 0
    while(idx < as.size) {
      val ai = as(idx)
      val bi = bs(idx)
      
      dot += ai*bi
      magA += ai*ai
      magB += bi*bi

      idx += 1
    }

    dot / (math.sqrt(magA)*math.sqrt(magB))
  }
  def cosineDiff(as: Array[Double], bs: Array[Double]): Double = 1.0 - cosineSimilarity(as, bs)

  def findSimilar(args: Array[String]) {
    if(args.size != 4) {
      Util.quit("error: expected -> index-csv method count query")
    }

    val index = new CSVIndex(args(0))
    val method = args(1)
    val numResults = args(2).toInt
    val query = args(3)

    if(!index.prob.contains(query)) {
      return
    }

    val comparison = method match {
      case "cos" => cosineSimilarity _
      case "cos-diff" => cosineDiff _
      case x => {
        Console.err.println(x)
        ???
      }
    }

    index.findSimilar(index.prob(query), comparison, numResults).foreach {
      case SimilarTerm(key, score, _) => {
        println(key+" "+score)
      }
    }
  }
  
  def findFake(args: Array[String]) {
    if(args.size != 3) {
      Util.quit("error: expected -> index-csv year count")
    }

    val index = new CSVIndex(args(0))
    val year = args(1).toInt
    val numResults = args(2).toInt

    ???
  }

  def plotMatching(args: Array[String]) {
    if(args.size < 2) {
      Util.quit("error: expected -> index-csv queries...")
    }

    val index = new CSVIndex(args(0))
    val queries = args.tail.map(_.toLowerCase)

    val keys = queries.map(q => {
      index.terms.filter(key => key.contains(q))
    }).flatten
    
    println(index.header)
    keys.foreach(q => {
      val res = index.prob(q)
      println(q+","+res.mkString(","))
    })
  }

  def plotExact(args: Array[String]) {
    if(args.size < 2) {
      Util.quit("error: expected -> index-csv queries...")
    }

    val index = new CSVIndex(args(0))
    val queries = args.tail
    
    println(index.header)
    queries.foreach(q => {
      index.prob.get(q) match {
        case Some(res) => println(q+","+res.mkString(","))
        case None => Console.err.println(q+" was not found!")
      }
    })
  }
}

class CSVIndex(val path: String) {
  val data = IO.fileLines(path).map(line => {
    val cells = line.split(" ")
    val term = cells(0)
    val tfv = cells.tail.map(_.toDouble).toArray
    (term, tfv)
  }).toMap
  val yearMax = {
    var vocabCount = new Array[Double](domain.size)
    data.values.foreach(varray => {
      vocabCount.indices.foreach(idx => {
        vocabCount(idx) += varray(idx)
      })
    })
    vocabCount
  }
  assert(yearMax.size == domain.size)
  val prob = data.mapValues(arr => {
    Array.tabulate(domain.size)(idx => {
      val tf = arr(idx)
      val maxTF = yearMax(idx)
      tf / maxTF
    })
  })

  def findSimilar(queryCurve: Array[Double], comparison: (Array[Double], Array[Double])=>Double, numResults: Int) = {
    val rankedList = new RankedList[SimilarTerm](numResults)

    prob.foreach {
      case (key, curve) => {
        rankedList.insert(SimilarTerm(key, comparison(queryCurve, curve), Array())) 
      }
    }

    rankedList.done
  }
  
  val header = "#,"+domain.mkString(",")
  def terms = data.keySet
  def domain = (1820 until 1920)
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
