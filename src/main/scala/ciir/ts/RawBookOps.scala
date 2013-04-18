package ciir.ts

import gnu.trove.map.hash.TIntIntHashMap



// meant to be run on Internet Archive books on swarm
// the id->path file is located at /work3/data/oca/text/ids-files.gz
object CountBooksByDate {
  var numBooks = 0
  var nonExistent = 0
  var badMetadata = 0
  var noLanguage = 0
  var nonEnglish = 0
  var noDate = 0

  var nodeID = 0
  var numNodes = 1
  def mine(i: Int) = ( i % numNodes == nodeID )

  def processBook(key: String, path: String): Option[Int] = {
    val metadata = XMLStream.simpleGetKeys(path, Set("identifier", "year", "date", "language"))

    // reject mismatched metadata
    val mid = metadata.getOrElse("identifier", "")
    if(mid != key) {
      badMetadata += 1
    }
    // reject not-obviously-english text
    metadata.get("language") match {
      case None => { noLanguage += 1; }
      case Some(lang) => {
        val ez = lang.trim.toLowerCase
        if(ez.isEmpty) {
          noLanguage += 1
        } else if(!ez.startsWith("eng")) {
          nonEnglish += 1
        }
      }
    }

    // get either date or year as an integer, prefering year
    val year = Vector(metadata.get("year"), metadata.get("date")).flatten.flatMap((str: String) => {
      val digits = str.filter(_.isDigit)
      if(digits.size == 4) // TODO
        Some(digits.toInt)
      else None
    }).headOption.getOrElse(-1)
    
    if(year == -1) {
      noDate += 1
      return None;
    }
    
    Some(year)
  }

  val MetadataExt = "_meta.xml.bz2"
  val BookExt = "_mbtei.xml.gz"

  def run(args: Array[String]) {
    val idPathFile = args(0)
    if(args.size > 1 && args(1) == "swarm") {
      assert(args.size == 4)
      nodeID = (args(2).toInt - 1) // turn from 1-based to 0-based
      numNodes = args(3).toInt
      assert(nodeID < numNodes && nodeID >= 0)
      Console.err.println("# Swarm Compute Node "+nodeID+"/"+numNodes)
    }
    var inputStream = IO.textInputStream(idPathFile)

    val startTime = System.currentTimeMillis()

    var lineNumber = 0
    IO.forLineInFile(idPathFile, line => {
      lineNumber += 1
      if(mine(lineNumber)) {
        line.split("\\s") match {
          case Array(key, path) => {
            val metadataExists = IO.fileExists(path)
            val bookPath = path.dropRight(MetadataExt.size)+BookExt
            val bookExists = IO.fileExists(bookPath)
            
            if(metadataExists && bookExists && path.endsWith(MetadataExt)) {
              numBooks += 1
              if(numBooks % 1000 == 0) { println("# "+numBooks) }
              processBook(key,path) match {
                case Some(yr) => {
                  println(key+" "+bookPath+" "+yr)
                }
                case None => { }
              }
            } else { nonExistent += 1 }
          }
          case _ => { }
        }
      } else {
        // the line in this file is not my responsibility
      }
    })

    val endTime = System.currentTimeMillis()

    Console.err.println("numBooks " + numBooks)
    Console.err.println("nonExistent " + nonExistent)
    Console.err.println("badMetadata " + badMetadata)
    Console.err.println("noLanguage " + noLanguage)
    Console.err.println("nonEnglish " + nonEnglish)
    Console.err.println("noDate " + noDate)
    Console.err.println("time " + (endTime - startTime))
  }

  def graph(args: Array[String]) {
    var dateCounts = new TIntIntHashMap()

    args.foreach(fileName => {
      if(!IO.fileExists(fileName)) {
        Console.err.println("Expected file we can open as first argument.")
      }

      IO.forLineInFile(fileName, line => {
        if(line.charAt(0) != '#') {
          line.split("\\s") match {
            case Array(_, dateStr) => {
              val date = dateStr.toInt
              
              // gutenberg's printing press, current date
              if(date >= 1436 && date <= 2013) {
                dateCounts.adjustOrPutValue(date, 1, 1)
              }
            }
            case _ => { }
          }
        }
      })
    })
    
    dateCounts.keys.sorted.foreach(date => {
      val counts = dateCounts.get(date)
      if(counts >= 30) {
        println(date+","+counts)
      }
    })
  }

  def stats(args: Array[String]) {
    var numBooks = 0
    var nonExistent = 0
    var badMetadata = 0
    var noLanguage = 0
    var nonEnglish = 0
    var noDate = 0
    var totalTime = 0
    
    args.foreach(fileName => {
      if(!IO.fileExists(fileName)) {
        Console.err.println("Expected file we can open as first argument.")
      }
      
      IO.forLineInFile(fileName, line => {
        if(line.charAt(0) != '#') {
          line.split("\\s") match {
            case Array(key, value) => {
              val count = value.toInt
              key match {
                case "numBooks" => numBooks += count
                case "nonExistent" => nonExistent += count
                case "badMetadata" => badMetadata += count
                case "noLanguage" => noLanguage += count
                case "nonEnglish" => nonEnglish += count
                case "noDate" => noDate += count
                case "time" => totalTime += count
                case _ => { }
              }
            }
            case _ => { }
          }
        }
      })
    })
    
    println("numBooks " + numBooks)
    println("nonExistent " + nonExistent)
    println("badMetadata " + badMetadata)
    println("noLanguage " + noLanguage)
    println("nonEnglish " + nonEnglish)
    println("noDate " + noDate)
    println("time " + totalTime+"ms")
  }

  def dateLists(args: Array[String]) {
    if(args.size < 2) {
      Util.quit("Expected arguments: outDir inputFiles...")
    }
    val outDir = args.head
    val inputFiles = args.tail
    
    // check input files
    val notFiles = inputFiles.filter(!IO.fileExists(_))
    if(notFiles.size > 0) {
      Util.quit("Bad file arguments: "+notFiles.mkString(","))
    }

    // make output directory
    var outDirFile = new java.io.File(outDir)
    if(!outDirFile.exists()) {
      outDirFile.mkdirs()
    }

    var outStreams = collection.mutable.Map[Int,java.io.PrintWriter]()
    inputFiles.foreach(fileName => {
      IO.forLineInFile(fileName, line => {
        if(!line.startsWith("java -jar")) {
          line.trim.split("\\s") match {
            case Array(id, yearText) => {
              val year = yearText.toInt
              if(year >= 1820 && year <= 1920) {
                if(!outStreams.contains(year)) {
                  outStreams(year) = IO.textOutputStream(outDir+"/"+yearText+".txt")
                }
                outStreams(year).println(id)
              }
            }
          }
        }
      })
    })
    outStreams.foreach {
      case (_, fp) => fp.close()
    }
  }
}

