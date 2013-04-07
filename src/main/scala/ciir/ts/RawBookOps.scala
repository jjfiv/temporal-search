package ciir.ts

class XMLStream(path: String) {
  var inputStream = Util.textInputStream(path)
  def close() { inputStream.close() }
  
  def peek: Option[Char] = {
    assert(inputStream.markSupported())
    inputStream.mark(1)
    val theValue = inputStream.read()
    inputStream.reset()
    
    if(theValue == -1) None
    else Some(theValue.toChar)
  }

  def done = peek match {
    case None => true
    case Some(_) => false
  }

  def skip() { inputStream.skip(1L) }

  def next(): Option[Char] = {
    val nextInt = inputStream.read()
    if(nextInt == -1) {
      None
    } else {
      Some(nextInt.toChar)
    }
  }

  def discardUntil(marker: Char) {
    while(true) {
      peek match {
        case None => { return }
        case Some(c) => {
          if(c == marker)
            return
          skip()
        }
      }
    }
  }
  def readUntil(marker: Char) = {
    var sb = new StringBuilder
    var done = false
    while(!done) {
      peek match {
        case None => { done = true }
        case Some(c) => {
          if(c == marker) {
            done = true
          } else {
            sb += c
            skip()
          }
        }
      }
    }
    sb.result
  }

  def nextTag: Option[String] = {
    discardUntil('<')
    skip()

    if(done) {
      None
    } else {
      val tag = readUntil('>');
      skip()
      Some(tag)
    }
  }
  def nextData: String = { readUntil('<') }
}

object QuickXML {
  def getValues(path: String, keys: Set[String]): Map[String,String] = {
    var mb = Map.newBuilder[String,String]
    var xmlStream = new XMLStream(path)

    try {
      var done = false
      while(!done) {
        xmlStream.nextTag match {
          case Some(tag) => {
            if(keys.contains(tag)) {
              val contents = xmlStream.nextData
              if(contents.size > 0) {
                mb += ((tag, contents))
              }
              assert(xmlStream.nextTag.getOrElse("") == "/"+tag)
            }
          }
          case None => { done = true }
        }
      }
    } finally {
      xmlStream.close()
    }

    mb.result()
  }
}

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
    val metadata = QuickXML.getValues(path, Set("identifier", "year", "date", "language"))

    // reject mismatched metadata
    val mid = metadata.getOrElse("identifier", "")
    if(mid != key) {
      //println((mid, key))
      badMetadata += 1
    }
    // reject not-obviously-english text
    metadata.get("language") match {
      case None => { noLanguage += 1; return None }
      case Some(lang) => {
        if(lang.toLowerCase != "english") {
          nonEnglish += 1
          return None;
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

  def run(args: Array[String]) {
    val idPathFile = args(0)
    if(args.size > 1 && args(1) == "swarm") {
      assert(args.size == 4)
      nodeID = (args(2).toInt - 1) // turn from 1-based to 0-based
      numNodes = args(3).toInt
      assert(nodeID < numNodes && nodeID >= 0)
      Console.err.println("Swarm Compute Node "+nodeID+"/"+numNodes)
    }
    var inputStream = Util.textInputStream(idPathFile)

    val startTime = System.currentTimeMillis()

    var lineNumber = 0
    Util.forLineInFile(idPathFile, line => {
      lineNumber += 1
      if(mine(lineNumber)) {
        line.split("\\s") match {
          case Array(key, path) => {
            val exists = Util.fileExists(path)
            if(exists) {
              numBooks += 1
              processBook(key,path) match {
                case Some(yr) => {
                  println(key + " "+yr)
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
    Console.err.println("time processing: " + (endTime - startTime)+ "ms")
  }

}

