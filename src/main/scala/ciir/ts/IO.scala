package ciir.ts

object IO {
  import java.io._
  import java.util.zip.{GZIPInputStream, GZIPOutputStream, ZipInputStream, ZipOutputStream}
  import org.apache.commons.compress.compressors.bzip2.{BZip2CompressorInputStream, BZip2CompressorOutputStream}

  def fileExists(fileName: String) = {
    if (fileName == null)
      false
    else new File(fileName).exists()
  }
  def dirExists(fileName: String) = {
    if(fileName == null)
      false
    else {
      val fp = new File(fileName)
      fp.exists() && fp.isDirectory()
    }
  }

  def requireDirectory(outDir: String) {
    val file = new File(outDir)
    if(!file.exists()) file.mkdirs()
    if(!file.isDirectory)
      Util.quit("couldn't get \""+outDir+"\" as a directory... it's a file?")
  }

  def binaryOutputStream(fn: String): DataOutputStream = {
    val fis = new FileOutputStream(fn)
    
    if(fn.endsWith(".gz")) {
      new DataOutputStream(new GZIPOutputStream(fis))
    } else if(fn.endsWith(".bz") || fn.endsWith(".bz2")) {
      new DataOutputStream(new BZip2CompressorOutputStream(fis))
    } else if(fn.endsWith(".zip")) {
      new DataOutputStream(new ZipOutputStream(fis))
    } else {
      new DataOutputStream(fis)
    }
  }
  def binaryInputStream(fn: String): DataInputStream = {
    val fis = new FileInputStream(fn)
    
    if(fn.endsWith(".gz")) {
      new DataInputStream(new GZIPInputStream(fis))
    } else if(fn.endsWith(".bz") || fn.endsWith(".bz2")) {
      new DataInputStream(new BZip2CompressorInputStream(fis))
    } else if(fn.endsWith(".zip")) {
      new DataInputStream(new ZipInputStream(fis))
    } else {
      new DataInputStream(fis)
    }
  }
  def textOutputStream(fn: String) = new PrintWriter(new OutputStreamWriter(binaryOutputStream(fn)))
  def textInputStream(fn: String) = new BufferedReader(new InputStreamReader(binaryInputStream(fn)))

  def fileLines(fn: String): Array[String] = {
    var lb = Array.newBuilder[String]
    forLineInFile(fn, line => lb += line)
    lb.result
  }

  def linesFromFile(fn: String, start: Int, count: Int): Array[String] = {
    val end = start + count
    
    var dest = new Array[String](count)
    var fp = textInputStream(fn)
    
    try {
      var line=0
      
      while(line < end) {
        val text = fp.readLine()
        if(text == null) {
          line = end
        } else if(line >= start) {
          dest(line - start) = text
        }
        line += 1
      }
    } finally {
      fp.close()
    }
    
    dest.filter(_ != null)
  }

  def forLineInFile(fn: String, op: String=>Unit) {
    var fp = textInputStream(fn)

    try {
      var done = false
      while(!done) {
        val line = fp.readLine()
        if(line == null) {
          done = true
        } else {
          op(line)
        }
      }
    } finally {
      fp.close()
    }
  }

  def printToFile(fileName: String, op: PrintWriter=>Unit) {
    var p = new PrintWriter(new File(fileName))
    try { op(p) } finally { p.close() }
  }

}

class CharacterStream(path: String) {
  private var fp = IO.textInputStream(path)
  private var buf = new collection.mutable.Queue[Char]()

  private def fgetc() = fp.read()
  // Try to make sure the internal buffer is of a given length
  // obviously, the real file may not cooperate
  private def tryEnsureLength(len: Int): Boolean = {
    while(buf.size < len) {
      val next = fgetc()
      if(next == -1) {
        return false; // can't make it the right length
      }
      buf += next.toChar
    }
    true
  }
  private def growBuffer() = tryEnsureLength(math.max(1,buf.size*2))
  private def fromBuf(idx: Int) = {
    // grow exponentiallly in size as we need more
    if(idx >= buf.size) {
      growBuffer()
    }

    // return the character or else EOF
    if(idx < buf.size) buf(idx) else -1
  }

  // file open/close
  def more() = peek != -1
  def done() = !more()
  def close() { 
    buf.clear()
    fp.close()
  }
  // count method
  def countWhile(pred: Char=>Boolean): Int = {
    var count = 0
    while(true) {
      val next = fromBuf(count)
      
      if(next == -1) {
        // if we hit an end of file, we have some result or nothing
        if(count == 0) { return -1 } else { return count }
      } else if(!pred(next.toChar)) {
        // if we hit a non-matching char we have some result, whether it's empty or not
        return count
      } else {
        count += 1
      }
    }
    -1
  }

  // peek methods
  def peek(): Int = fromBuf(0)
  def peek(count: Int): Option[String] = {
    if(!tryEnsureLength(count)) return None
    Some(buf.take(count).mkString)
  }
  def peekUntil(marker: Char): Option[String] = {
    // grow the buffer until it contains the marker we're looking for
    while(!buf.contains(marker)) {
      if(!growBuffer()) {
        return None
      }
    }
    return Some(buf.takeWhile(_ != marker).mkString)
  }
  def peekWhile(pred: Char=>Boolean): Option[String] = {
    countWhile(pred) match {
      case -1 => None
      case 0 => Some("")
      case x => peek(x)
    }
  }
  def peekMatches(str: String): Boolean = {
    peek(str.size) match {
      case None => false
      case Some(next) => next == str
    }
  }

  // if you subsist upon peeks alone, you'll be dropping often
  def drop(amt: Int = 1) {
    if(amt > 0) {
      buf = buf.drop(amt)
    }
  }
  def dropUntil(marker: Char) {
    peekUntil(marker)
    buf = buf.dropWhile(_ != marker)
  }
  def dropIncluding(marker: Char) {
    dropUntil(marker); drop()
  }
  def dropWhile(pred: Char=>Boolean) {
    drop(countWhile(pred))
  }
  // get methods
  def get(): Int = {
    if(buf.size > 0) {
      val ch = buf.head
      drop(1)
      ch
    } else fgetc()
  }
  def get(count: Int): Option[String] = {
    val res = peek(count)
    drop(count)
    res
  }
  def getUntil(marker: Char): Option[String] = {
    val res = peekUntil(marker)
    dropUntil(marker)
    res
  }
  def getWhile(pred: Char=>Boolean): Option[String] = {
    countWhile(pred) match {
      case -1 => None
      case 0 => Some("")
      case x => get(x)
    }
  }
  def getIncluding(marker: Char): Option[String] = {
    getUntil(marker) match {
      case None => None
      case Some(str) => {
        if(peek == -1) None
        else {
          Some(str + get().toChar)
        }
      }
    }
  }
  def getLine(): Option[String] = {
    val newlineChars = Set('\r','\n')
    val str = getWhile(!newlineChars.contains(_))
    if(peek == '\r') drop(1)
    if(peek == '\n') drop(1)
    str
  }
}

object XMLStream {
  def simpleGetKeys(path: String, keys: Set[String]): Map[String,String] = {
    var mb = Map.newBuilder[String,String]
    var xmlStream: XMLStream = null

    try {
      xmlStream = new XMLStream(path)
      var done = false
      while(!done) {
        xmlStream.nextTag() match {
          case Some(tag) => {
            val tagName = tag.takeWhile(!_.isWhitespace)
            if(keys.contains(tagName)) {
              val contents = xmlStream.nextData
              if(contents.nonEmpty) {
                mb += ((tag, contents))
              }
              // if we've collected all the metadata we need
              if(mb.result.size == keys.size) {
                done = true
              }
              assert(xmlStream.nextTag.getOrElse("") == "/"+tagName)
            }
          }
          case None => { done = true }
        }
      }
    } catch {
      case eof: java.io.EOFException => {
        Console.err.println("# ignore EOFException - bad metadata read")
      }
    } finally {
      if(xmlStream != null) {
        xmlStream.close()
      }
    }

    mb.result()
  }
}

class XMLStream(path: String) extends CharacterStream(path) {
  def nextTag(): Option[String] = {
    dropIncluding('<')

    val tag = getUntil('>');
    drop() // the '>' character
    tag
  }
  def nextData(): String = { getUntil('<').getOrElse("") }
}

