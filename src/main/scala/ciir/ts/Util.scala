package ciir.ts

import org.lemurproject.galago.tupleflow.StreamCreator

object Util {
  import java.io._

  def fileExists(fileName: String) = {
    if (fileName == null)
      false
    else new File(fileName).exists()
  }

  def binaryOutputStream(fn: String) = StreamCreator.openOutputStream(fn)
  def binaryInputStream(fn: String) = StreamCreator.openInputStream(fn)
  def textOutputStream(fn: String) = new BufferedWriter(new OutputStreamWriter(binaryOutputStream(fn)))
  def textInputStream(fn: String) = new BufferedReader(new InputStreamReader(StreamCreator.openInputStream(fn)))

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

  def timed[A](desc: String, block: =>A): A = {
    val ti = System.currentTimeMillis
    val result = block
    val tf = System.currentTimeMillis
    println("timed: \""+desc+"\" took "+(tf-ti)+"ms!")
    result
  }

  def fraction(a: Int, b: Int): Double = {
    if(b == 0) {
      0.0
    } else {
      a.toDouble / b.toDouble
    }
  }
  def harmonicMean(a: Double, b: Double) = { (2*a*b) / (a+b) }
  def loopUntil(max: Int)(op: Int=>Unit) {
    var i=0
    while(i < max) {
      op(i)
      i+=1
    }
  }
}


