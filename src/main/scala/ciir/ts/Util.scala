package ciir.ts


object Util {
  val runtime = Runtime.getRuntime()

  def KiB(bytes: Long) = bytes >>> 10
  def MiB(bytes: Long) = bytes >>> 20
  def GiB(bytes: Long) = bytes >>> 30

  def suggestGC() {
    runtime.gc()
    runtime.gc()
  }

  // not accurate because of garbage collection
  def usedMem = runtime.totalMemory - runtime.freeMemory

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
  def hex(num: Int) = "0x%08x".format(num)
  def quit(msg: String, code: Int = -1) {
    Console.err.println(msg)
    sys.exit(code)
  }

  def runCLI(prompt: String, quitCmd: String)(onInput: String=>Unit) {
    var done = false
    while(!done) {
      val line = scala.Console.readLine(prompt)
      if(line == null || line == quitCmd) {
        done = true
      } else {
        onInput(line)
      }
    }
  }
  
  def whileCLI(prompt: String)(onInput: String=>Boolean) {
    var done = false
    while(!done) {
      val line = scala.Console.readLine(prompt)
      if(line == null) {
        done = true
      } else {
        done = !onInput(line)
      }
    }
  }
}


