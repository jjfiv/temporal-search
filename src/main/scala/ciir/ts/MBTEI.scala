package ciir.ts

object MBTEI {
  def words(path: String): Array[String] = {
    var words = Array.newBuilder[String]
    
    var reader = new MBTEIWordReader(path)
    try {
      var done = false
      while(!done) {
        reader.nextWord() match {
          case None => { done = true }
          case Some(str) => { words += str }
        }
      }
    } catch {
      case eof: java.io.EOFException => {
        return Array()
      }
    } finally {
      reader.close()
    }
    words.result()
  }
}

class MBTEIWordReader(path: String) extends CharacterStream(path) {
  private def skipIncluding(marker: Char) {
    while(true) {
      val ch = get()
      if(ch == -1 || ch == marker) return
      
      // in XML, all escaped quotes are of the form &quot;
      // no backslash handling needed, afaik
      if(ch == '"') dropUntil('"')
    }
  }
  private def readUntil(marker: Char): String = {
    var sb = new StringBuilder
    var done = false
    while(!done) {
      val x = get()
      val ch = x.toChar
      if(x == -1 || ch == marker) {
        done = true
      } else if(ch == '&') {
        readUntil(';') match {
          case "amp" => sb += '&'
          case "lt" => sb += '<'
          case "gt" => sb += '>'
          case "quot" => sb += '"'
          case _ => { }
        }
      } else {
        sb += ch
      }
    }
    sb.result
  }
  def nextTag(): Option[String] = {
    skipIncluding('<')
    getWhile(ch => ch != '>' && !ch.isWhitespace)
  }
  def nextWord(): Option[String] = {
    // looks for <w.*> and saves contents
    while(true) {
      skipIncluding('<')

      if(done()) return None

      if(peekMatches("w ") || peekMatches("w>")) {
        dropUntil('>')
        // ignore empty tags: <w></w>
        val contents = readUntil('<')
        if(contents.size != 0)
          return Some(contents)
      }
    }
    None
  }
}

