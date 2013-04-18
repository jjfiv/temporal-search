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

  def dateLists(args: Array[String]) {
    if(args.size < 2) {
      Util.quit("Expected arguments: outDir idPathDateList")
    }

    val outDir = args(0)
    val inputFile = args(1)

    if(!IO.fileExists(inputFile)) {
      Util.quit("No such file: "+inputFile)
    }

    // make output directory
    var outDirFile = new java.io.File(outDir)
    if(!outDirFile.exists()) {
      outDirFile.mkdirs()
    }

    var outStreams = collection.mutable.Map[Int,java.io.PrintWriter]()

    IO.forLineInFile(inputFile, line => {
      line.trim.split("\\s") match {
        case Array(_, path, yearText) => {
          // don't much care for id if we have the path itself
          val year = yearText.toInt
          if(year >= 1820 && year <= 1920) {
            if(!outStreams.contains(year)) {
              outStreams(year) = IO.textOutputStream(outDir+"/"+yearText+".list")
            }
            outStreams(year).println(path)
          }
        }
      }
    })

    outStreams.foreach {
      case (_, fp) => fp.close()
    }
  }
}

