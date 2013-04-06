package ciir.ts

import org.lemurproject.galago.tupleflow.Parameters

object Util {
  import java.io._

  def argsAsJSON(argv: Array[String]): Parameters = {
    val (files, args) = argv.partition(fileExists)

    var parameters = new Parameters()
    
    // load all json files given
    files.map(f => { parameters.copyFrom(Parameters.parse(new File(f))) })

    // read parameters from arguments next
    parameters.copyFrom(new Parameters(args));

    parameters
  }

  def fileExists(fileName: String) = {
    new File(fileName).exists()
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


