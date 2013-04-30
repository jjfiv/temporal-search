package ciir.ts

object MBTEI {
  def words(path: String): Array[String] = {
    var words = Array.newBuilder[String]
    
    var reader = new MBTEIReader(path)
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
  private def parseMetadata(book: MBTEIReader): Map[String, String] = {
    var mmap = new collection.mutable.HashMap[String,String]
    var done = false

    var count = 0
    
    while(!done) {
      book.nextTag() match {
        case None | Some("text") | Some("/metadata") => {
          done = true
        }
        case Some(tag) => {
          count += 1
          val contents = book.readTag
          // if the map already contains this tag, append to it
          if(mmap.contains(tag)) {
            val prev = mmap(tag)
            mmap(tag) = prev+"\n"+contents
          } else {
            mmap(tag) = contents
          }
        }
      }
    }
    
    mmap.toMap
  }
  def parse(path: String, keepMetadata: Boolean, keepText: Boolean, keepWords: Boolean) = {
    var book: MBTEIReader = null
    var metadata = Map[String,String]()
    var words = Array.newBuilder[String]
    var text = new StringBuilder
    var done = false

    try {
      book = new MBTEIReader(path)
      while(!done) {
        book.nextTag() match {
          case Some("metadata") => if(keepMetadata) metadata = parseMetadata(book)
          case Some("w") => {
            val contents = book.readTag.trim
            if(contents.nonEmpty) {
              if (keepWords) { words += contents }
              if (keepText) { text.append(contents).append(" ") }
            }
          }
          case Some("pb") | Some("lb") => if(keepText) {
            // drop previous space if any
            if(text.nonEmpty && text.last == ' ') {
              text.setCharAt(text.size-1, '\n')
            } else {
              text += '\n'
            }
          }
          case Some(_) => { } // unhandled tags
          case None => { done = true } // eof
        }
      }
    } finally {
      if(book != null) {
        book.close()
      }
    }

    (metadata, words.result, text.result)
  }
}

class MBTEIReader(path: String) extends CharacterStream(path) {
  private def skipIncluding(marker: Char) {
    while(true) {
      val ch = get()
      if(ch == -1 || ch.toChar == marker) return
      
      // in XML, all escaped quotes are of the form &quot;
      // no backslash handling needed, afaik
      if(ch == '"') dropIncluding('"')
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
  def readTag(): String = {
    dropIncluding('>')
    readUntil('<')
  }
  def nextWord(): Option[String] = {
    // looks for <w.*> and saves contents
    while(true) {
      nextTag() match {
        case Some("w") => {
          val contents = readTag()
          if(contents.size != 0)
            return Some(contents)
        }
        case Some(t) => { }
        case None => return None
      }
    }
    None // make scalac happy
  }
}

