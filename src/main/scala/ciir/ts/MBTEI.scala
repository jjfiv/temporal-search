package ciir.ts

object MBTEI {
  def idFromPath(path: String) = {
    val id = path.stripSuffix("_mbtei.xml.gz")
    val loc = id.lastIndexOf('/')+1 // -1 or appropriate, -> 0 or appropriate
    id.substring(loc)
  }
  def words(path: String): Array[String] = {
    var words = Array.newBuilder[String]
    
    var reader = new MBTEIWordReader(path)
    try {
      var done = false
      while(!done) {
        reader.next() match {
          case None => { done = true }
          case Some(str) => { words += str }
        }
      }
    } finally {
      reader.close()
    }
    words.result()
  }
}

class MBTEIWordReader(path: String) {
  private var fp = IO.textInputStream(path)

  def close() = fp.close()
  private def getc() = fp.read()
  private def skipQuoteContents() {
    // in XML, all escaped quotes are of the form &quot;
    // no backslash handling needed, afaik
    while(true) {
      val ch = getc() 
      if(ch == -1 || ch == '"')
        return
    }
  }
  private def skipUntil(marker: Char) {
    while(true) {
      val ch = getc()
      if(ch == -1 || ch == marker) return
      if(ch == '"') skipQuoteContents()
    }
  }
  private def readUntil(marker: Char): String = {
    var sb = new StringBuilder
    var done = false
    while(!done) {
      val x = getc()
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
  def next(): Option[String] = {
    // looks for <w.*> and saves contents
    while(true) {
      skipUntil('<')

      val first = getc()
      val second = getc()
      if(first == -1 || second == -1) return None

      val ch0 = first.toChar
      val ch1 = second.toChar
      
      if(ch0 == 'w' && (ch1.isWhitespace || ch1 == '>')) {
        if(ch1 != '>')
          skipUntil('>')
        
        // ignore empty tags: <w></w>
        val contents = readUntil('<')
        if(contents.size != 0)
          return Some(contents)
      }
    }
    None
  }
}

