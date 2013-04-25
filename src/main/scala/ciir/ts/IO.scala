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

  private def getc() = fp.read()
  // Try to make sure the internal buffer is of a given length
  // obviously, the real file may not cooperate
  private def tryEnsureLength(len: Int) {
    var idx = buf.size
    while(idx < len) {
      val next = getc()
      if(next == -1) {
        return false; // can't make it the right length
      }
      buf += next.toChar
    }
    true
  }
  private def growBuffer() = tryEnsureLength(buf.size*2)
  private def fromBuf(idx: Int) = {
    // grow exponentiallly in size as we need more
    if(idx >= buf.size) growBuffer()

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

  // peek methods
  def peek: Int = fromBuf(0)
  def peek(count: Int): Option[String] = {
    if(!tryEnsureLength(count)) return None
    Some(buf.take(count).toString)
  }
  def peekUntil(marker: Char): Option[String] = {
    // grow the buffer until it contains the marker we're looking for
    while(!buf.contains(marker)) {
      if(!growBuffer()) {
        return None
      }
    }
    return Some(buf.takeWhile(_ != marker))
  }
  def peekMatches(str: String): Boolean = {
    peek(str.size) match {
      case None => false
      case Some(next) => next == str
    }
  }

  // if you subsist upon peeks alone, you'll be dropping often
  def drop(amt: Int) {
    buf = buf.drop(amt)
  }
  def dropUntil(marker: Char) {
    buf = buf.dropWhile(_ != marker)
  }
  def dropIncluding(marker: Char) {
    dropUntil(marker); drop()
  }
  // get methods
  def get(): Int = {
    if(tryEnsureLength(1)) buf.dequeue() else -1
  }
  def get(count: Int): Option[String] = {
    val res = peek(count)
    drop(count)
    res
  }
  def getUntil(marker: Char): Option[String] = {
    val res = peekUntil(marker)
    dropUntil(marker)
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

    if(done()) {
      None
    } else {
      val tag = getUntil('>');
      drop() // the '>' character
      tag
    }
  }
  def nextData(): String = { getUntil('<').getOrElse("") }
}

