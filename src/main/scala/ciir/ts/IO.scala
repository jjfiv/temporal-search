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
  def textOutputStream(fn: String) = new BufferedWriter(new OutputStreamWriter(binaryOutputStream(fn)))
  def textInputStream(fn: String) = new BufferedReader(new InputStreamReader(binaryInputStream(fn)))

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

object XMLStream {
  def simpleGetKeys(path: String, keys: Set[String]): Map[String,String] = {
    var mb = Map.newBuilder[String,String]
    var xmlStream = new XMLStream(path)

    try {
      var done = false
      while(!done) {
        xmlStream.nextTag match {
          case Some(tag) => {
            val tagName = tag.takeWhile(!_.isWhitespace)
            if(keys.contains(tagName)) {
              val contents = xmlStream.nextData
              if(contents.size > 0) {
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
    } finally {
      xmlStream.close()
    }

    mb.result()
  }
}

class XMLStream(path: String) {
  var inputStream = IO.textInputStream(path)
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

