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
